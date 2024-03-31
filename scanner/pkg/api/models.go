package api

type Page[T any] struct {
	items    []T          `validate:"required,dive,required"`
	metadata PageMetadata `validate:"required,dive,required"`
}

type PageMetadata struct {
	next string
}

type File struct {
	path string `validate:"required"`
}
