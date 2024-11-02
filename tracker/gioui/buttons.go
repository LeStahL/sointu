package gioui

import (
	"gioui.org/layout"
	"gioui.org/unit"
	"gioui.org/widget/material"
	"gioui.org/x/component"
	"github.com/vsariola/sointu/tracker"
	"github.com/vsariola/sointu/tracker/gioui/patch"
	patched "github.com/vsariola/sointu/tracker/gioui/patch/material"
)

type (
	TipClickable struct {
		Clickable patch.Clickable
		TipArea   component.TipArea
	}

	ActionClickable struct {
		Action tracker.Action
		TipClickable
	}

	TipIconButtonStyle struct {
		TipArea         *component.TipArea
		IconButtonStyle patched.IconButtonStyle
		Tooltip         component.Tooltip
	}

	BoolClickable struct {
		Clickable patch.Clickable
		TipArea   component.TipArea
		Bool      tracker.Bool
	}
)

func NewActionClickable(a tracker.Action) *ActionClickable {
	return &ActionClickable{
		Action: a,
	}
}

func NewBoolClickable(b tracker.Bool) *BoolClickable {
	return &BoolClickable{
		Bool: b,
	}
}

func Tooltip(th *material.Theme, tip string) component.Tooltip {
	tooltip := component.PlatformTooltip(th, tip)
	tooltip.Bg = black
	return tooltip
}

func ActionIcon(gtx C, th *material.Theme, w *ActionClickable, icon []byte, tip string) TipIconButtonStyle {
	ret := TipIcon(th, &w.TipClickable, icon, tip)
	for w.Clickable.Clicked(gtx) {
		w.Action.Do()
	}
	if !w.Action.Allowed() {
		ret.IconButtonStyle.Color = disabledTextColor
	}
	return ret
}

func TipIcon(th *material.Theme, w *TipClickable, icon []byte, tip string) TipIconButtonStyle {
	iconButtonStyle := patched.IconButton(th, &w.Clickable, widgetForIcon(icon), "")
	iconButtonStyle.Color = primaryColor
	iconButtonStyle.Background = transparent
	iconButtonStyle.Inset = layout.UniformInset(unit.Dp(6))
	return TipIconButtonStyle{
		TipArea:         &w.TipArea,
		IconButtonStyle: iconButtonStyle,
		Tooltip:         Tooltip(th, tip),
	}
}

func ToggleIcon(gtx C, th *material.Theme, w *BoolClickable, offIcon, onIcon []byte, offTip, onTip string) TipIconButtonStyle {
	icon := offIcon
	tip := offTip
	if w.Bool.Value() {
		icon = onIcon
		tip = onTip
	}
	for w.Clickable.Clicked(gtx) {
		w.Bool.Toggle()
	}
	ibStyle := patched.IconButton(th, &w.Clickable, widgetForIcon(icon), "")
	ibStyle.Background = transparent
	ibStyle.Inset = layout.UniformInset(unit.Dp(6))
	ibStyle.Color = primaryColor
	if !w.Bool.Enabled() {
		ibStyle.Color = disabledTextColor
	}
	return TipIconButtonStyle{
		TipArea:         &w.TipArea,
		IconButtonStyle: ibStyle,
		Tooltip:         Tooltip(th, tip),
	}
}

func (t *TipIconButtonStyle) Layout(gtx C) D {
	return t.TipArea.Layout(gtx, t.Tooltip, t.IconButtonStyle.Layout)
}

func ActionButton(gtx C, th *material.Theme, w *ActionClickable, text string) patched.ButtonStyle {
	for w.Clickable.Clicked(gtx) {
		w.Action.Do()
	}
	ret := patched.Button(th, &w.Clickable, text)
	ret.Color = th.Palette.Fg
	if !w.Action.Allowed() {
		ret.Color = disabledTextColor
	}
	ret.Background = transparent
	ret.Inset = layout.UniformInset(unit.Dp(6))
	return ret
}

func ToggleButton(gtx C, th *material.Theme, b *BoolClickable, text string) patched.ButtonStyle {
	for b.Clickable.Clicked(gtx) {
		b.Bool.Toggle()
	}
	ret := patched.Button(th, &b.Clickable, text)
	ret.Background = transparent
	ret.Inset = layout.UniformInset(unit.Dp(6))
	if b.Bool.Value() {
		ret.Color = th.Palette.ContrastFg
		ret.Background = th.Palette.Fg
	} else {
		ret.Color = th.Palette.Fg
		ret.Background = transparent
	}
	return ret
}

func LowEmphasisButton(th *material.Theme, w *patch.Clickable, text string) patched.ButtonStyle {
	ret := patched.Button(th, w, text)
	ret.Color = th.Palette.Fg
	ret.Background = transparent
	ret.Inset = layout.UniformInset(unit.Dp(6))
	return ret
}

func HighEmphasisButton(th *material.Theme, w *patch.Clickable, text string) patched.ButtonStyle {
	ret := patched.Button(th, w, text)
	ret.Color = th.Palette.ContrastFg
	ret.Background = th.Palette.Fg
	ret.Inset = layout.UniformInset(unit.Dp(6))
	return ret
}
