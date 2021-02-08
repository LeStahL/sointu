package tracker

import (
	"image"

	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"golang.org/x/exp/shiny/materialdesign/icons"
)

type C = layout.Context
type D = layout.Dimensions

func (t *Tracker) updateInstrumentScroll() {
	if t.CurrentInstrument > 7 {
		t.InstrumentDragList.List.Position.First = t.CurrentInstrument - 7
	} else {
		t.InstrumentDragList.List.Position.First = 0
	}
}

func (t *Tracker) layoutInstruments(gtx C) D {
	btnStyle := material.IconButton(t.Theme, t.NewInstrumentBtn, widgetForIcon(icons.ContentAdd))
	btnStyle.Background = transparent
	btnStyle.Inset = layout.UniformInset(unit.Dp(6))
	if t.song.Patch.TotalVoices() < 32 {
		btnStyle.Color = primaryColor
	} else {
		btnStyle.Color = disabledTextColor
	}
	return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx C) D {
			return layout.Flex{}.Layout(
				gtx,
				layout.Flexed(1, t.layoutInstrumentNames),
				layout.Rigid(func(gtx C) D {
					return layout.E.Layout(gtx, btnStyle.Layout)
				}),
			)
		}),
		layout.Rigid(t.layoutInstrumentHeader),
		layout.Flexed(1, t.layoutInstrumentEditor))
}

func (t *Tracker) layoutInstrumentHeader(gtx C) D {
	headerBg := func(gtx C) D {
		paint.FillShape(gtx.Ops, instrumentSurfaceColor, clip.Rect{
			Max: gtx.Constraints.Min,
		}.Op())
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}
	header := func(gtx C) D {
		deleteInstrumentBtnStyle := material.IconButton(t.Theme, t.DeleteInstrumentBtn, widgetForIcon(icons.ActionDelete))
		deleteInstrumentBtnStyle.Background = transparent
		deleteInstrumentBtnStyle.Inset = layout.UniformInset(unit.Dp(6))
		if len(t.song.Patch.Instruments) > 1 {
			deleteInstrumentBtnStyle.Color = primaryColor
		} else {
			deleteInstrumentBtnStyle.Color = disabledTextColor
		}

		return layout.Flex{Axis: layout.Horizontal, Alignment: layout.Middle}.Layout(gtx,
			layout.Rigid(Label("Voices: ", white)),
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				maxRemain := 32 - t.song.Patch.TotalVoices() + t.song.Patch.Instruments[t.CurrentInstrument].NumVoices
				if maxRemain < 0 {
					maxRemain = 0
				}
				t.InstrumentVoices.Value = t.song.Patch.Instruments[t.CurrentInstrument].NumVoices
				numStyle := NumericUpDown(t.Theme, t.InstrumentVoices, 0, maxRemain)
				gtx.Constraints.Min.Y = gtx.Px(unit.Dp(20))
				gtx.Constraints.Min.X = gtx.Px(unit.Dp(70))
				dims := numStyle.Layout(gtx)
				t.SetInstrumentVoices(t.InstrumentVoices.Value)
				return dims
			}),
			layout.Flexed(1, func(gtx C) D { return layout.Dimensions{Size: gtx.Constraints.Min} }),
			layout.Rigid(deleteInstrumentBtnStyle.Layout))
	}
	for t.DeleteInstrumentBtn.Clicked() {
		t.DeleteInstrument()
	}
	return layout.Stack{Alignment: layout.Center}.Layout(gtx,
		layout.Expanded(headerBg),
		layout.Stacked(header))
}

func (t *Tracker) layoutInstrumentNames(gtx C) D {
	element := func(gtx C, i int) D {
		gtx.Constraints.Min.Y = gtx.Px(unit.Dp(36))
		gtx.Constraints.Min.X = gtx.Px(unit.Dp(30))
		grabhandle := LabelStyle{Text: "", ShadeColor: black, Color: white, FontSize: unit.Sp(10), Alignment: layout.Center}
		if i == t.CurrentInstrument {
			grabhandle.Text = ":::"
		}
		label := func(gtx C) D {
			if i == t.CurrentInstrument {
				for _, ev := range t.InstrumentNameEditor.Events() {
					_, ok := ev.(widget.SubmitEvent)
					if ok {
						t.InstrumentNameEditor = &widget.Editor{SingleLine: true, Submit: true, Alignment: text.Middle} // TODO: is there any other way to defocus the editor
						break
					}
				}
				if n := t.song.Patch.Instruments[t.CurrentInstrument].Name; n != t.InstrumentNameEditor.Text() {
					t.InstrumentNameEditor.SetText(n)
				}
				editor := material.Editor(t.Theme, t.InstrumentNameEditor, "Instr")
				editor.Color = instrumentNameColor
				editor.HintColor = instrumentNameHintColor
				editor.TextSize = unit.Dp(12)
				dims := layout.Center.Layout(gtx, editor.Layout)
				t.SetInstrumentName(t.InstrumentNameEditor.Text())
				return dims
			}
			text := t.song.Patch.Instruments[i].Name
			if text == "" {
				text = "Instr"
			}
			labelStyle := LabelStyle{Text: text, ShadeColor: black, Color: white, FontSize: unit.Sp(12)}
			return layout.Center.Layout(gtx, labelStyle.Layout)
		}
		return layout.Inset{Left: unit.Dp(6), Right: unit.Dp(6)}.Layout(gtx, func(gtx C) D {
			return layout.Flex{Axis: layout.Vertical, Alignment: layout.Middle}.Layout(gtx,
				layout.Rigid(grabhandle.Layout),
				layout.Rigid(label),
			)
		})
	}

	instrumentList := FilledDragList(t.Theme, t.InstrumentDragList, len(t.song.Patch.Instruments), element, t.SwapInstruments)
	instrumentList.SelectedColor = instrumentSurfaceColor
	instrumentList.HoverColor = instrumentHoverColor
	instrumentList.SurfaceColor = transparent

	t.InstrumentDragList.SelectedItem = t.CurrentInstrument
	defer op.Save(gtx.Ops).Load()
	pointer.PassOp{Pass: true}.Add(gtx.Ops)
	dims := instrumentList.Layout(gtx)
	if t.CurrentInstrument != t.InstrumentDragList.SelectedItem {
		t.CurrentInstrument = t.InstrumentDragList.SelectedItem
		if l := len(t.song.Patch.Instruments[t.CurrentInstrument].Units); t.CurrentUnit >= l {
			t.CurrentUnit = l - 1
		}
		op.InvalidateOp{}.Add(gtx.Ops)
	}
	return dims
}
func (t *Tracker) layoutInstrumentEditor(gtx C) D {
	for t.AddUnitBtn.Clicked() {
		t.AddUnit()
	}
	addUnitBtnStyle := material.IconButton(t.Theme, t.AddUnitBtn, widgetForIcon(icons.ContentAdd))
	addUnitBtnStyle.Inset = layout.UniformInset(unit.Dp(4))
	margin := layout.UniformInset(unit.Dp(2))

	element := func(gtx C, i int) D {
		gtx.Constraints = layout.Exact(image.Pt(gtx.Px(unit.Dp(120)), gtx.Px(unit.Dp(20))))
		u := t.song.Patch.Instruments[t.CurrentInstrument].Units[i]
		labelStyle := LabelStyle{Text: u.Type, ShadeColor: black, Color: white, Font: labelDefaultFont, FontSize: unit.Sp(12)}
		if labelStyle.Text == "" {
			labelStyle.Text = "---"
			labelStyle.Alignment = layout.Center
		}
		return labelStyle.Layout(gtx)
	}

	unitList := FilledDragList(t.Theme, t.UnitDragList, len(t.song.Patch.Instruments[t.CurrentInstrument].Units), element, t.SwapUnits)

	t.UnitDragList.SelectedItem = t.CurrentUnit
	return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
		layout.Rigid(func(gtx C) D {
			return layout.Stack{Alignment: layout.SE}.Layout(gtx,
				layout.Expanded(func(gtx C) D {
					dims := unitList.Layout(gtx)
					if t.CurrentUnit != t.UnitDragList.SelectedItem {
						t.CurrentUnit = t.UnitDragList.SelectedItem
						op.InvalidateOp{}.Add(gtx.Ops)
					}
					return dims
				}),
				layout.Stacked(func(gtx C) D {
					return margin.Layout(gtx, addUnitBtnStyle.Layout)
				}))
		}),
		layout.Rigid(t.layoutUnitEditor))
}