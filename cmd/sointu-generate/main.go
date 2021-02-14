// +build ignore
package main

import (
	"encoding/binary"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/vsariola/sointu/tracker"
)

type riffVisitor interface {
	Enter(listType string)
	Leave(listType string)
	Visit(filePos int, chunkType string, data []byte)
}

type gmDlsVisitor struct {
	outputFile io.Writer
	entry      tracker.GmDlsEntry
	dataLength int
	count      int
}

func (v *gmDlsVisitor) Enter(listType string) {
	if listType == "wave" {
		v.entry = tracker.GmDlsEntry{}
		v.dataLength = 0
	}
}

func (v *gmDlsVisitor) Leave(listType string) {
	if listType == "wave" {
		if v.entry.LoopLength == 0 {
			v.entry.LoopStart = v.dataLength/2 - 1
			v.entry.LoopLength = 1
		}
		if v.entry.Name == "" {
			v.entry.Name = fmt.Sprintf("#%v", v.count)
		}
		fmt.Fprintf(v.outputFile, "\t{Start: %v, LoopStart: %v, LoopLength: %v, SuggestedTranspose: %v, Name: \"%v\"},\n", v.entry.Start, v.entry.LoopStart, v.entry.LoopLength, v.entry.SuggestedTranspose, v.entry.Name)
		v.count++
	}
}

func (v *gmDlsVisitor) Visit(filePos int, chunkType string, data []byte) {
	switch chunkType {
	case "wsmp":
		v.entry.SuggestedTranspose = 60 - int(binary.LittleEndian.Uint16(data[4:6]))
		numLoops := binary.LittleEndian.Uint32(data[16:20])
		if numLoops > 0 {
			v.entry.LoopStart = int(binary.LittleEndian.Uint32(data[28:32]))
			v.entry.LoopLength = int(binary.LittleEndian.Uint32(data[32:36]))
		}
	case "data":
		v.entry.Start = filePos / 2
		v.dataLength = len(data)
	case "INAM":
		v.entry.Name = string(data[:len(data)-1])
	}
}

func readChunk(filePos int, reader io.Reader, visitor riffVisitor) int {
	var chunkType [4]byte
	n, err := io.ReadFull(reader, chunkType[:])
	check(err)
	if n < 4 {
		panic("did not get full 4 bytes for a chunktype")
	}
	filePos += n
	chunkTypeString := string(chunkType[:])
	var chunkLength int32
	check(binary.Read(reader, binary.LittleEndian, &chunkLength))
	filePos += 4
	if chunkTypeString == "RIFF" || chunkTypeString == "LIST" {
		end := filePos + int(chunkLength)
		var listType [4]byte
		n, err := io.ReadFull(reader, listType[:])
		check(err)
		if n < 4 {
			panic("did not get full 4 bytes for a subchunkname")
		}
		filePos += n
		listTypeString := string(listType[:])
		visitor.Enter(listTypeString)
		for filePos < end {
			filePos = readChunk(filePos, reader, visitor)
		}
		visitor.Leave(listTypeString)
	} else {
		data := make([]byte, chunkLength)
		n, err := reader.Read(data)
		check(err)
		if n < int(chunkLength) {
			panic(fmt.Sprintf("chunk %v was supposed to be %v bytes, got %v bytes", chunkTypeString, chunkLength, n))
		}
		visitor.Visit(filePos, chunkTypeString, data)
		filePos += n
	}
	if chunkLength&1 == 1 {
		io.CopyN(ioutil.Discard, reader, 1)
		filePos++
	}
	return filePos
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	inputFile, err := os.Open("C:\\Windows\\System32\\drivers\\gm.dls")
	check(err)
	defer inputFile.Close()
	//_, myname, _, _ := runtime.Caller(0)
	//outputFilePath := path.Join(path.Dir(myname), "gmdlsentries.go")
	//check(err)
	outputFile, err := os.Create("gmdlsentries.go")
	check(err)
	defer outputFile.Close()
	fmt.Fprintln(outputFile, "// Code generated by go generate; DO NOT EDIT.")
	fmt.Fprintln(outputFile, "package tracker")
	fmt.Fprintln(outputFile, "")
	fmt.Fprintln(outputFile, "var gmDlsEntries = []GmDlsEntry{")
	readChunk(0, inputFile, &gmDlsVisitor{outputFile: outputFile})
	fmt.Fprintln(outputFile, "}")
}
