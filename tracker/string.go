package tracker

type (
	String struct {
		StringData
	}

	StringData interface {
		Value() string
		setValue(string)
		change(kind string) func()
	}

	FilePath          Model
	InstrumentName    Model
	InstrumentComment Model
	UnitSearch        Model
	UnitComment       Model
)

func (v String) Set(value string) {
	if v.Value() != value {
		defer v.change("Set")()
		v.setValue(value)
	}
}

// Model methods

func (m *Model) FilePath() *FilePath                   { return (*FilePath)(m) }
func (m *Model) InstrumentName() *InstrumentName       { return (*InstrumentName)(m) }
func (m *Model) InstrumentComment() *InstrumentComment { return (*InstrumentComment)(m) }
func (m *Model) UnitSearch() *UnitSearch               { return (*UnitSearch)(m) }
func (m *Model) UnitComment() *UnitComment             { return (*UnitComment)(m) }

// FilePathString

func (v *FilePath) String() String            { return String{v} }
func (v *FilePath) Value() string             { return v.d.FilePath }
func (v *FilePath) setValue(value string)     { v.d.FilePath = value }
func (v *FilePath) change(kind string) func() { return func() {} }

// UnitSearchString

func (v *UnitSearch) String() String { return String{v} }
func (v *UnitSearch) Value() string {
	// return current unit type string if not searching
	if !v.d.UnitSearching {
		if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) {
			return ""
		}
		if v.d.UnitIndex < 0 || v.d.UnitIndex >= len(v.d.Song.Patch[v.d.InstrIndex].Units) {
			return ""
		}
		return v.d.Song.Patch[v.d.InstrIndex].Units[v.d.UnitIndex].Type
	} else {
		return v.d.UnitSearchString
	}
}
func (v *UnitSearch) setValue(value string) {
	v.d.UnitSearchString = value
	v.d.UnitSearching = true
}
func (v *UnitSearch) change(kind string) func() { return func() {} }

// InstrumentNameString

func (v *InstrumentName) String() String {
	return String{v}
}

func (v *InstrumentName) Value() string {
	if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) {
		return ""
	}
	return v.d.Song.Patch[v.d.InstrIndex].Name
}

func (v *InstrumentName) setValue(value string) {
	if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) {
		return
	}
	v.d.Song.Patch[v.d.InstrIndex].Name = value
}

func (v *InstrumentName) change(kind string) func() {
	return (*Model)(v).change("InstrumentNameString."+kind, PatchChange, MinorChange)
}

// InstrumentComment

func (v *InstrumentComment) String() String {
	return String{v}
}

func (v *InstrumentComment) Value() string {
	if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) {
		return ""
	}
	return v.d.Song.Patch[v.d.InstrIndex].Comment
}

func (v *InstrumentComment) setValue(value string) {
	if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) {
		return
	}
	v.d.Song.Patch[v.d.InstrIndex].Comment = value
}

func (v *InstrumentComment) change(kind string) func() {
	return (*Model)(v).change("InstrumentComment."+kind, PatchChange, MinorChange)
}

// UnitComment

func (v *UnitComment) String() String { return String{v} }
func (v *UnitComment) Value() string {
	if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) ||
		v.d.UnitIndex < 0 || v.d.UnitIndex >= len(v.d.Song.Patch[v.d.InstrIndex].Units) {
		return ""
	}
	return v.d.Song.Patch[v.d.InstrIndex].Units[v.d.UnitIndex].Comment
}

func (v *UnitComment) setValue(value string) {
	if v.d.InstrIndex < 0 || v.d.InstrIndex >= len(v.d.Song.Patch) ||
		v.d.UnitIndex < 0 || v.d.UnitIndex >= len(v.d.Song.Patch[v.d.InstrIndex].Units) {
		return
	}
	v.d.Song.Patch[v.d.InstrIndex].Units[v.d.UnitIndex].Comment = value
}

func (v *UnitComment) change(kind string) func() {
	return (*Model)(v).change("UnitComment."+kind, PatchChange, MinorChange)
}
