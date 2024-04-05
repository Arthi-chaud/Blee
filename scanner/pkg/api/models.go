package api

type Page[T any] struct {
	Items    []T          `validate:"required,dive,required" json:"items"`
	Metadata PageMetadata `json:"metadata"`
}

type PageMetadata struct {
	Next  string `json:"next"`
	Count uint64 `json:"count"`
}

type File struct {
	Path string `validate:"required" json:"path"`
}
