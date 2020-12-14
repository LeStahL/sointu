package main

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"gopkg.in/yaml.v3"

	"github.com/vsariola/sointu/go4k"
	"github.com/vsariola/sointu/go4k/audio/oto"
	"github.com/vsariola/sointu/go4k/bridge"
	"github.com/vsariola/sointu/go4k/compiler"
)

func main() {
	write := flag.Bool("w", false, "Do not output to standard output; (over)write files on disk instead.")
	list := flag.Bool("l", false, "Do not output to standard output; list files that change if -w is applied.")
	help := flag.Bool("h", false, "Show help.")
	play := flag.Bool("p", false, "Play the input songs.")
	asmOut := flag.Bool("a", false, "Output the song as .asm file, to standard output unless otherwise specified.")
	tmplDir := flag.String("t", "", "Output the song as by parsing the templates in directory, to standard output unless otherwise specified.")
	jsonOut := flag.Bool("j", false, "Output the song as .json file, to standard output unless otherwise specified.")
	yamlOut := flag.Bool("y", false, "Output the song as .yml file, to standard output unless otherwise specified.")
	headerOut := flag.Bool("c", false, "Output .h C header file, to standard output unless otherwise specified.")
	exactLength := flag.Bool("e", false, "When outputting the C header file, calculate the exact length of song by rendering it once. Only useful when using SPEED opcodes.")
	rawOut := flag.Bool("r", false, "Output the rendered song as .raw stereo float32 buffer, to standard output unless otherwise specified.")
	directory := flag.String("d", "", "Directory where to output all files. The directory and its parents are created if needed. By default, everything is placed in the same directory where the original song file is.")
	hold := flag.Int("o", -1, "New value to be used as the hold value")
	targetArch := flag.String("arch", runtime.GOARCH, "Target architecture. Defaults to OS architecture. Possible values: 386, amd64")
	targetOs := flag.String("os", runtime.GOOS, "Target OS. Defaults to current OS. Possible values: windows, darwin, linux (anything else is assumed linuxy)")
	flag.Usage = printUsage
	flag.Parse()
	if flag.NArg() == 0 || *help {
		flag.Usage()
		os.Exit(0)
	}
	var comp *compiler.Compiler
	if !*asmOut && !*jsonOut && !*rawOut && !*headerOut && !*play && !*yamlOut && *tmplDir == "" {
		*play = true // if the user gives nothing to output, then the default behaviour is just to play the file
	}
	needsRendering := *play || *exactLength || *rawOut
	needsCompile := *headerOut || *asmOut
	if needsCompile {
		var err error
		comp, err = compiler.New()
		if err != nil {
			fmt.Fprintf(os.Stderr, `error creating compiler: %v`, err)
			os.Exit(1)
		}
		comp.Amd64 = *targetArch == "amd64"
		comp.OS = *targetOs
	}
	process := func(filename string) error {
		output := func(extension string, contents []byte) error {
			if !*write && !*list {
				fmt.Print(string(contents))
				return nil
			}
			dir, name := filepath.Split(filename)
			if *directory != "" {
				dir = *directory
			}
			name = strings.TrimSuffix(name, filepath.Ext(name)) + extension
			f := filepath.Join(dir, name)
			original, err := ioutil.ReadFile(f)
			if err == nil {
				if bytes.Compare(original, contents) == 0 {
					return nil // no need to update
				}
			}
			if *list {
				fmt.Println(f)
			}
			if *write {
				if err := os.MkdirAll(dir, os.ModePerm); err != nil {
					return fmt.Errorf("Could not create output directory %v: %v", dir, err)
				}
				err := ioutil.WriteFile(f, contents, 0644)
				if err != nil {
					return fmt.Errorf("Could not write file %v: %v", f, err)
				}
			}
			return nil
		}
		inputBytes, err := ioutil.ReadFile(filename)
		if err != nil {
			return fmt.Errorf("Could not read file %v: %v", filename, err)
		}
		var song go4k.Song
		if errJSON := json.Unmarshal(inputBytes, &song); errJSON != nil {
			if errYaml := yaml.Unmarshal(inputBytes, &song); errYaml != nil {
				return fmt.Errorf("The song could not be parsed as .json (%v) or .yml (%v)", errJSON, errYaml)
			}
		}
		var buffer []float32
		if needsRendering {
			synth, err := bridge.Synth(song.Patch)
			if err != nil {
				return fmt.Errorf("Could not create synth based on the patch: %v", err)
			}
			buffer, err = go4k.Play(synth, song) // render the song to calculate its length
			if err != nil {
				return fmt.Errorf("go4k.Play failed: %v", err)
			}
		}
		if *play {
			player, err := oto.NewPlayer()
			if err != nil {
				return fmt.Errorf("Error creating oto player: %v", err)
			}
			defer player.Close()
			if err := player.Play(buffer); err != nil {
				return fmt.Errorf("Error playing: %v", err)
			}
		}
		if *hold > -1 {
			err = song.UpdateHold(byte(*hold))
			if err != nil {
				return fmt.Errorf("error updating the hold value of the song: %v", err)
			}
		}
		var compiledPlayer map[string]string
		if needsCompile {
			maxSamples := 0 // 0 means it is calculated automatically
			if *exactLength {

				maxSamples = len(buffer) / 2
			}
			var err error
			compiledPlayer, err = comp.Player(&song, maxSamples)
			if err != nil {
				return fmt.Errorf("compiling player failed: %v", err)
			}
		}
		if *headerOut {
			if err := output(".h", []byte(compiledPlayer["h"])); err != nil {
				return fmt.Errorf("Error outputting header file: %v", err)
			}
		}
		if *asmOut {
			if err := output(".asm", []byte(compiledPlayer["asm"])); err != nil {
				return fmt.Errorf("Error outputting asm file: %v", err)
			}
		}
		if *jsonOut {
			jsonSong, err := json.Marshal(song)
			if err != nil {
				return fmt.Errorf("Could not marshal the song as json file: %v", err)
			}
			if err := output(".json", jsonSong); err != nil {
				return fmt.Errorf("Error outputting JSON file: %v", err)
			}
		}
		if *yamlOut {
			yamlSong, err := yaml.Marshal(song)
			if err != nil {
				return fmt.Errorf("Could not marshal the song as yaml file: %v", err)
			}
			if err := output(".yml", yamlSong); err != nil {
				return fmt.Errorf("Error outputting yaml file: %v", err)
			}
		}
		if *rawOut {
			buf := new(bytes.Buffer)
			err := binary.Write(buf, binary.LittleEndian, buffer)
			if err != nil {
				return fmt.Errorf("Could not binary write the float32 buffer to a byte buffer: %v", err)
			}
			if err := output(".raw", buf.Bytes()); err != nil {
				return fmt.Errorf("Error outputting raw audio file: %v", err)
			}
		}
		return nil
	}
	retval := 0
	for _, param := range flag.Args() {
		if info, err := os.Stat(param); err == nil && info.IsDir() {
			asmfiles, err := filepath.Glob(filepath.Join(param, "*.asm"))
			if err != nil {
				fmt.Fprintf(os.Stderr, "Could not glob the path %v for asm files: %v\n", param, err)
				retval = 1
				continue
			}
			jsonfiles, err := filepath.Glob(filepath.Join(param, "*.json"))
			if err != nil {
				fmt.Fprintf(os.Stderr, "Could not glob the path %v for json files: %v\n", param, err)
				retval = 1
				continue
			}
			ymlfiles, err := filepath.Glob(filepath.Join(param, "*.yml"))
			if err != nil {
				fmt.Fprintf(os.Stderr, "Could not glob the path %v for yml files: %v\n", param, err)
				retval = 1
				continue
			}
			files := append(asmfiles, jsonfiles...)
			files = append(files, ymlfiles...)
			for _, file := range files {
				err := process(file)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Could not process file %v: %v\n", file, err)
					retval = 1
				}
			}
		} else {
			err := process(param)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Could not process file %v: %v\n", param, err)
				retval = 1
			}
		}
	}
	os.Exit(retval)
}

func printUsage() {
	fmt.Fprintf(os.Stderr, "Sointu command line utility for processing .asm/.json song files.\nUsage: %s [flags] [path ...]\n", os.Args[0])
	flag.PrintDefaults()
}
