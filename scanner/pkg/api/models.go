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
	Id   string `validate:"required" json:"id"`
	Path string `validate:"required" json:"path"`
}

type NewExtraResponse struct {
	ExtraId string `validate:"required" json:"extra_id"`
}

type NewChapterResponse struct {
	ExtraId string `validate:"required" json:"extra_id"`
}

type NewMovieResponse struct {
	MovieId    string   `validate:"required" json:"movie_id"`
	ChaptersId []string `validate:"required,dive,required" json:"chapters_id"`
}
