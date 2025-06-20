package gioui

import (
	"fmt"
	"image"
	"image/color"
	"strconv"

	"gioui.org/gesture"
	"gioui.org/layout"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
	"github.com/vsariola/sointu/tracker"
	"github.com/vsariola/sointu/version"
	"golang.org/x/exp/shiny/materialdesign/icons"
)

type SongPanel struct {
	SongSettingsExpander *Expander
	ScopeExpander        *Expander
	LoudnessExpander     *Expander
	PeakExpander         *Expander

	WeightingTypeBtn *Clickable
	OversamplingBtn  *Clickable

	BPM            *NumberInput
	RowsPerPattern *NumberInput
	RowsPerBeat    *NumberInput
	Step           *NumberInput
	SongLength     *NumberInput

	Scope *OscilloscopeState

	MenuBar *MenuBar
	PlayBar *PlayBar
}

func NewSongPanel(model *tracker.Model) *SongPanel {
	ret := &SongPanel{
		BPM:            NewNumberInput(model.BPM()),
		RowsPerPattern: NewNumberInput(model.RowsPerPattern()),
		RowsPerBeat:    NewNumberInput(model.RowsPerBeat()),
		Step:           NewNumberInput(model.Step()),
		SongLength:     NewNumberInput(model.SongLength()),
		Scope:          NewOscilloscope(model),
		MenuBar:        NewMenuBar(model),
		PlayBar:        NewPlayBar(model),

		WeightingTypeBtn: &Clickable{},
		OversamplingBtn:  &Clickable{},

		SongSettingsExpander: &Expander{Expanded: true},
		ScopeExpander:        &Expander{},
		LoudnessExpander:     &Expander{},
		PeakExpander:         &Expander{},
	}
	return ret
}

func (s *SongPanel) Update(gtx C, t *Tracker) {
	for s.WeightingTypeBtn.Clicked(gtx) {
		t.Model.DetectorWeighting().SetValue((t.DetectorWeighting().Value() + 1) % int(tracker.NumWeightingTypes))
	}
	for s.OversamplingBtn.Clicked(gtx) {
		t.Model.Oversampling().SetValue(!t.Oversampling().Value())
	}
}

func (s *SongPanel) Layout(gtx C, t *Tracker) D {
	s.Update(gtx, t)
	return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx C) D {
			return s.MenuBar.Layout(gtx, t)
		}),
		layout.Rigid(func(gtx C) D {
			return s.PlayBar.Layout(gtx, t.Theme)
		}),
		layout.Rigid(func(gtx C) D {
			return s.layoutSongOptions(gtx, t)
		}),
	)
}

func (t *SongPanel) layoutSongOptions(gtx C, tr *Tracker) D {
	paint.FillShape(gtx.Ops, tr.Theme.SongPanel.Bg, clip.Rect(image.Rect(0, 0, gtx.Constraints.Max.X, gtx.Constraints.Max.Y)).Op())

	var weightingTxt string
	switch tracker.WeightingType(tr.Model.DetectorWeighting().Value()) {
	case tracker.KWeighting:
		weightingTxt = "K-weight (LUFS)"
	case tracker.AWeighting:
		weightingTxt = "A-weight"
	case tracker.CWeighting:
		weightingTxt = "C-weight"
	case tracker.NoWeighting:
		weightingTxt = "No weight (RMS)"
	}

	weightingBtn := Btn(tr.Theme, &tr.Theme.Button.Text, t.WeightingTypeBtn, weightingTxt)

	oversamplingTxt := "Sample peak"
	if tr.Model.Oversampling().Value() {
		oversamplingTxt = "True peak"
	}
	oversamplingBtn := Btn(tr.Theme, &tr.Theme.Button.Text, t.OversamplingBtn, oversamplingTxt)

	return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx C) D {
			return t.SongSettingsExpander.Layout(gtx, tr.Theme, "Song",
				func(gtx C) D {
					return Label(tr.Theme, &tr.Theme.SongPanel.RowHeader, strconv.Itoa(tr.BPM().Value())+" BPM").Layout(gtx)
				},
				func(gtx C) D {
					return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "BPM", NumUpDown(tr.Theme, t.BPM, "BPM").Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Song length", NumUpDown(tr.Theme, t.SongLength, "Song Length").Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Rows per pat", NumUpDown(tr.Theme, t.RowsPerPattern, "Rows per pattern").Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Rows per beat", NumUpDown(tr.Theme, t.RowsPerBeat, "Rows per beat").Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Cursor step", NumUpDown(tr.Theme, t.Step, "Cursor step").Layout)
						}),
					)
				})
		}),
		layout.Rigid(func(gtx C) D {
			return t.LoudnessExpander.Layout(gtx, tr.Theme, "Loudness",
				func(gtx C) D {
					return Label(tr.Theme, &tr.Theme.SongPanel.RowHeader, fmt.Sprintf("%.1f dB", tr.Model.DetectorResult().Loudness[tracker.LoudnessShortTerm])).Layout(gtx)
				},
				func(gtx C) D {
					return layout.Flex{Axis: layout.Vertical, Alignment: layout.End}.Layout(gtx,
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Momentary", dbLabel(tr.Theme, tr.Model.DetectorResult().Loudness[tracker.LoudnessMomentary]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Short term", dbLabel(tr.Theme, tr.Model.DetectorResult().Loudness[tracker.LoudnessShortTerm]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Integrated", dbLabel(tr.Theme, tr.Model.DetectorResult().Loudness[tracker.LoudnessIntegrated]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Max. momentary", dbLabel(tr.Theme, tr.Model.DetectorResult().Loudness[tracker.LoudnessMaxMomentary]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Max. short term", dbLabel(tr.Theme, tr.Model.DetectorResult().Loudness[tracker.LoudnessMaxShortTerm]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							gtx.Constraints.Min.X = 0
							return weightingBtn.Layout(gtx)
						}),
					)
				},
			)
		}),
		layout.Rigid(func(gtx C) D {
			return t.PeakExpander.Layout(gtx, tr.Theme, "Peaks",
				func(gtx C) D {
					maxPeak := max(tr.Model.DetectorResult().Peaks[tracker.PeakShortTerm][0], tr.Model.DetectorResult().Peaks[tracker.PeakShortTerm][1])
					return dbLabel(tr.Theme, maxPeak).Layout(gtx)
				},
				func(gtx C) D {
					return layout.Flex{Axis: layout.Vertical, Alignment: layout.End}.Layout(gtx,
						// no need to show momentary peak, it does not have too much meaning
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Short term L", dbLabel(tr.Theme, tr.Model.DetectorResult().Peaks[tracker.PeakShortTerm][0]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Short term R", dbLabel(tr.Theme, tr.Model.DetectorResult().Peaks[tracker.PeakShortTerm][1]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Integrated L", dbLabel(tr.Theme, tr.Model.DetectorResult().Peaks[tracker.PeakIntegrated][0]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							return layoutSongOptionRow(gtx, tr.Theme, "Integrated R", dbLabel(tr.Theme, tr.Model.DetectorResult().Peaks[tracker.PeakIntegrated][1]).Layout)
						}),
						layout.Rigid(func(gtx C) D {
							gtx.Constraints.Min.X = 0
							return oversamplingBtn.Layout(gtx)
						}),
					)
				},
			)
		}),
		layout.Flexed(1, func(gtx C) D {
			return t.ScopeExpander.Layout(gtx, tr.Theme, "Oscilloscope", func(gtx C) D { return D{} }, Scope(t.Scope, tr.SignalAnalyzer().Waveform(), tr.Theme).Layout)
		}),
		layout.Rigid(Label(tr.Theme, &tr.Theme.SongPanel.Version, version.VersionOrHash).Layout),
	)
}

func dbLabel(th *Theme, value tracker.Decibel) LabelWidget {
	ret := Label(th, &th.SongPanel.RowValue, fmt.Sprintf("%.1f dB", value))
	if value >= 0 {
		ret.Color = th.SongPanel.ErrorColor
	}
	return ret
}

func layoutSongOptionRow(gtx C, th *Theme, label string, widget layout.Widget) D {
	leftSpacer := layout.Spacer{Width: unit.Dp(6), Height: unit.Dp(24)}.Layout
	rightSpacer := layout.Spacer{Width: unit.Dp(6)}.Layout

	return layout.Flex{Axis: layout.Horizontal, Alignment: layout.Middle}.Layout(gtx,
		layout.Rigid(leftSpacer),
		layout.Rigid(Label(th, &th.SongPanel.RowHeader, label).Layout),
		layout.Flexed(1, func(gtx C) D { return D{Size: gtx.Constraints.Min} }),
		layout.Rigid(widget),
		layout.Rigid(rightSpacer),
	)
}

type Expander struct {
	Expanded bool
	click    gesture.Click
}

func (e *Expander) Update(gtx C) {
	for ev, ok := e.click.Update(gtx.Source); ok; ev, ok = e.click.Update(gtx.Source) {
		switch ev.Kind {
		case gesture.KindClick:
			e.Expanded = !e.Expanded
		}
	}
}

func (e *Expander) Layout(gtx C, th *Theme, title string, smallWidget, largeWidget layout.Widget) D {
	e.Update(gtx)
	return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx C) D { return e.layoutHeader(gtx, th, title, smallWidget) }),
		layout.Rigid(func(gtx C) D {
			if e.Expanded {
				return largeWidget(gtx)
			}
			return D{}
		}),
		layout.Rigid(func(gtx C) D {
			px := max(gtx.Dp(unit.Dp(1)), 1)
			paint.FillShape(gtx.Ops, color.NRGBA{255, 255, 255, 3}, clip.Rect(image.Rect(0, 0, gtx.Constraints.Max.X, px)).Op())
			return D{Size: image.Pt(gtx.Constraints.Max.X, px)}
		}),
	)
}

func (e *Expander) layoutHeader(gtx C, th *Theme, title string, smallWidget layout.Widget) D {
	return layout.Background{}.Layout(gtx,
		func(gtx C) D {
			defer clip.Rect(image.Rect(0, 0, gtx.Constraints.Min.X, gtx.Constraints.Min.Y)).Push(gtx.Ops).Pop()
			// add click op
			e.click.Add(gtx.Ops)
			return D{Size: image.Pt(gtx.Constraints.Min.X, gtx.Constraints.Min.Y)}
		},
		func(gtx C) D {
			leftSpacer := layout.Spacer{Width: unit.Dp(6), Height: unit.Dp(24)}.Layout
			return layout.Flex{Axis: layout.Horizontal, Alignment: layout.Middle}.Layout(gtx,
				layout.Rigid(leftSpacer),
				layout.Rigid(Label(th, &th.SongPanel.Expander, title).Layout),
				layout.Flexed(1, func(gtx C) D { return D{Size: gtx.Constraints.Min} }),
				layout.Rigid(func(gtx C) D {
					if !e.Expanded {
						return smallWidget(gtx)
					}
					return D{}
				}),
				layout.Rigid(func(gtx C) D {
					// draw icon
					icon := icons.NavigationExpandMore
					if e.Expanded {
						icon = icons.NavigationExpandLess
					}
					gtx.Constraints.Min = image.Pt(gtx.Dp(unit.Dp(24)), gtx.Dp(unit.Dp(24)))
					return widgetForIcon(icon).Layout(gtx, th.SongPanel.Expander.Color)
				}),
			)
		},
	)
}

type MenuBar struct {
	Clickables []Clickable
	Menus      []Menu

	fileMenuItems []MenuItem
	editMenuItems []MenuItem
	midiMenuItems []MenuItem

	panicHint string
	PanicBtn  *BoolClickable
}

func NewMenuBar(model *tracker.Model) *MenuBar {
	ret := &MenuBar{
		Clickables: make([]Clickable, 3),
		Menus:      make([]Menu, 3),
		PanicBtn:   NewBoolClickable(model.Panic()),
		panicHint:  makeHint("Panic", " (%s)", "PanicToggle"),
	}
	ret.fileMenuItems = []MenuItem{
		{IconBytes: icons.ContentClear, Text: "New Song", ShortcutText: keyActionMap["NewSong"], Doer: model.NewSong()},
		{IconBytes: icons.FileFolder, Text: "Open Song", ShortcutText: keyActionMap["OpenSong"], Doer: model.OpenSong()},
		{IconBytes: icons.ContentSave, Text: "Save Song", ShortcutText: keyActionMap["SaveSong"], Doer: model.SaveSong()},
		{IconBytes: icons.ContentSave, Text: "Save Song As...", ShortcutText: keyActionMap["SaveSongAs"], Doer: model.SaveSongAs()},
		{IconBytes: icons.ImageAudiotrack, Text: "Export Wav...", ShortcutText: keyActionMap["ExportWav"], Doer: model.Export()},
	}
	if canQuit {
		ret.fileMenuItems = append(ret.fileMenuItems, MenuItem{IconBytes: icons.ActionExitToApp, Text: "Quit", ShortcutText: keyActionMap["Quit"], Doer: model.RequestQuit()})
	}
	ret.editMenuItems = []MenuItem{
		{IconBytes: icons.ContentUndo, Text: "Undo", ShortcutText: keyActionMap["Undo"], Doer: model.Undo()},
		{IconBytes: icons.ContentRedo, Text: "Redo", ShortcutText: keyActionMap["Redo"], Doer: model.Redo()},
		{IconBytes: icons.ImageCrop, Text: "Remove unused data", ShortcutText: keyActionMap["RemoveUnused"], Doer: model.RemoveUnused()},
	}
	for input := range model.MIDI.InputDevices {
		ret.midiMenuItems = append(ret.midiMenuItems, MenuItem{
			IconBytes: icons.ImageControlPoint,
			Text:      input.String(),
			Doer:      model.SelectMidiInput(input),
		})
	}
	return ret
}

func (t *MenuBar) Layout(gtx C, tr *Tracker) D {
	gtx.Constraints.Max.Y = gtx.Dp(unit.Dp(36))
	gtx.Constraints.Min.Y = gtx.Dp(unit.Dp(36))

	panicBtnStyle := ToggleIcon(gtx, tr.Theme, t.PanicBtn, icons.AlertErrorOutline, icons.AlertError, t.panicHint, t.panicHint)
	if t.PanicBtn.Bool.Value() {
		panicBtnStyle.IconButtonStyle.Color = tr.Theme.SongPanel.ErrorColor
	}
	flex := layout.Flex{Axis: layout.Horizontal, Alignment: layout.End}
	fileFC := layout.Rigid(tr.layoutMenu(gtx, "File", &t.Clickables[0], &t.Menus[0], unit.Dp(200), t.fileMenuItems...))
	editFC := layout.Rigid(tr.layoutMenu(gtx, "Edit", &t.Clickables[1], &t.Menus[1], unit.Dp(200), t.editMenuItems...))
	midiFC := layout.Rigid(tr.layoutMenu(gtx, "MIDI", &t.Clickables[2], &t.Menus[2], unit.Dp(200), t.midiMenuItems...))
	panicFC := layout.Flexed(1, func(gtx C) D { return layout.E.Layout(gtx, panicBtnStyle.Layout) })
	if len(t.midiMenuItems) > 0 {
		return flex.Layout(gtx, fileFC, editFC, midiFC, panicFC)
	}
	return flex.Layout(gtx, fileFC, editFC, panicFC)
}

type PlayBar struct {
	RewindBtn  *ActionClickable
	PlayingBtn *BoolClickable
	RecordBtn  *BoolClickable
	FollowBtn  *BoolClickable
	LoopBtn    *BoolClickable
	// Hints
	rewindHint                  string
	playHint, stopHint          string
	recordHint, stopRecordHint  string
	followOnHint, followOffHint string
	loopOffHint, loopOnHint     string
}

func NewPlayBar(model *tracker.Model) *PlayBar {
	ret := &PlayBar{
		LoopBtn:    NewBoolClickable(model.LoopToggle()),
		RecordBtn:  NewBoolClickable(model.IsRecording()),
		FollowBtn:  NewBoolClickable(model.Follow()),
		PlayingBtn: NewBoolClickable(model.Playing()),
		RewindBtn:  NewActionClickable(model.PlaySongStart()),
	}
	ret.rewindHint = makeHint("Rewind", "\n(%s)", "PlaySongStartUnfollow")
	ret.playHint = makeHint("Play", " (%s)", "PlayCurrentPosUnfollow")
	ret.stopHint = makeHint("Stop", " (%s)", "StopPlaying")
	ret.recordHint = makeHint("Record", " (%s)", "RecordingToggle")
	ret.stopRecordHint = makeHint("Stop", " (%s)", "RecordingToggle")
	ret.followOnHint = makeHint("Follow on", " (%s)", "FollowToggle")
	ret.followOffHint = makeHint("Follow off", " (%s)", "FollowToggle")
	ret.loopOffHint = makeHint("Loop off", " (%s)", "LoopToggle")
	ret.loopOnHint = makeHint("Loop on", " (%s)", "LoopToggle")
	return ret
}

func (pb *PlayBar) Layout(gtx C, th *Theme) D {
	rewindBtnStyle := ActionIcon(gtx, th, pb.RewindBtn, icons.AVFastRewind, pb.rewindHint)
	playBtnStyle := ToggleIcon(gtx, th, pb.PlayingBtn, icons.AVPlayArrow, icons.AVStop, pb.playHint, pb.stopHint)
	recordBtnStyle := ToggleIcon(gtx, th, pb.RecordBtn, icons.AVFiberManualRecord, icons.AVFiberSmartRecord, pb.recordHint, pb.stopRecordHint)
	noteTrackBtnStyle := ToggleIcon(gtx, th, pb.FollowBtn, icons.ActionSpeakerNotesOff, icons.ActionSpeakerNotes, pb.followOffHint, pb.followOnHint)
	loopBtnStyle := ToggleIcon(gtx, th, pb.LoopBtn, icons.NavigationArrowForward, icons.AVLoop, pb.loopOffHint, pb.loopOnHint)

	return Surface{Gray: 37}.Layout(gtx, func(gtx C) D {
		return layout.Flex{Axis: layout.Horizontal, Alignment: layout.Middle}.Layout(gtx,
			layout.Flexed(1, playBtnStyle.Layout),
			layout.Rigid(rewindBtnStyle.Layout),
			layout.Rigid(recordBtnStyle.Layout),
			layout.Rigid(noteTrackBtnStyle.Layout),
			layout.Rigid(loopBtnStyle.Layout),
		)
	})
}
