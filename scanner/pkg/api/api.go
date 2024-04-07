package api

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"

	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/Arthi-chaud/Blee/scanner/pkg/models"
	"github.com/go-playground/validator/v10"
	"github.com/goccy/go-json"
	"github.com/kpango/glg"
)

func HealthCheck(config config.Config) error {
	_, err := request("GET", "/", nil, config)
	return err
}

func GetAllKnownPaths(config config.Config) ([]string, error) {
	next := "/files"
	filePaths := []string{}
	validate := validator.New(validator.WithRequiredStructEnabled())

	for len(next) != 0 {
		res, err := request("GET", next, nil, config)
		if err != nil {
			return []string{}, err
		}
		var page Page[File] = Page[File]{}
		err = json.Unmarshal([]byte(res), &page)
		if err != nil {
			return []string{}, err
		}
		if err = validate.Struct(page); err != nil {
			return []string{}, err
		}
		for _, item := range page.Items {
			filePaths = append(filePaths, item.Path)
		}
		next = page.Metadata.Next
	}
	return filePaths, nil
}

func GetFileByPath(path string, c config.Config) (File, error) {
	res, err := request("GET", fmt.Sprintf("/files?path=%s", url.QueryEscape(path)), nil, c)
	if err != nil {
		return File{}, err
	}
	validate := validator.New(validator.WithRequiredStructEnabled())
	var page Page[File] = Page[File]{}
	if err = json.Unmarshal([]byte(res), &page); err != nil {
		return File{}, err
	}
	if err = validate.Struct(page); err != nil {
		return File{}, err
	}
	if len(page.Items) == 0 {
		return File{}, errors.New("File Not Found")
	}
	return page.Items[0], nil
}

func DeleteFile(fileUuid string, c config.Config) error {
	_, err := request("DELETE", fmt.Sprintf("/files/%s", fileUuid), nil, c)
	return err
}

func UpdateFile(fileUuid string, dto *models.UpdateFileDto, config config.Config) error {
	serialized, err := json.Marshal(dto)
	if err != nil {
		return err
	}
	_, err = request("POST", fmt.Sprintf("/files/%s", fileUuid), bytes.NewBuffer(serialized), config)
	if err != nil {
		return err
	}
	return nil
}

func SaveMovie(movie *models.NewMovieDto, config config.Config) (NewMovieResponse, error) {
	serialized, err := json.Marshal(movie)
	var newMovie = NewMovieResponse{}
	if err != nil {
		return newMovie, err
	}
	res, err := request("POST", "/movies", bytes.NewBuffer(serialized), config)
	if err != nil {
		return newMovie, err
	}
	err = json.Unmarshal([]byte(res), &newMovie)
	return newMovie, err
}

func SaveExtra(movie *models.NewExtraDto, config config.Config) (NewExtraResponse, error) {
	serialized, err := json.Marshal(movie)
	var newExtra = NewExtraResponse{}
	if err != nil {
		return newExtra, err
	}
	res, err := request("POST", "/extras", bytes.NewBuffer(serialized), config)
	if err != nil {
		return newExtra, err
	}
	err = json.Unmarshal([]byte(res), &newExtra)
	return newExtra, err
}

func SaveMovieThumbnail(movieUuid string, thumbnail io.Reader, config config.Config) error {
	_, err := request("POST", fmt.Sprintf("/movies/{%s}/thumbnail", movieUuid), thumbnail, config)
	return err
}

func SaveChapterThumbnail(chapterUuid string, thumbnail io.Reader, config config.Config) error {
	_, err := request("POST", fmt.Sprintf("/chapters/{%s}/thumbnail", chapterUuid), thumbnail, config)
	return err
}

func SaveExtraThumbnail(extraUuid string, thumbnail io.Reader, config config.Config) error {
	_, err := request("POST", fmt.Sprintf("/extras/{%s}/thumbnail", extraUuid), thumbnail, config)
	return err
}

func request(method string, url string, body io.Reader, config config.Config) (string, error) {
	client := &http.Client{}
	req, _ := http.NewRequest(method, fmt.Sprintf("%s%s", config.ApiUrl, url), body)
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	req.Header.Set("x-api-key", config.ApiKey)
	resp, err := client.Do(req)

	if err != nil {
		glg.Fatalln(err)
		return "", err
	}
	defer resp.Body.Close()
	b, err := io.ReadAll(resp.Body)
	if resp.StatusCode >= 400 {
		glg.Fatalln(string(b))
		return "", err
	}
	if err != nil {
		glg.Fatalln(err)
		return "", err
	}
	return string(b), nil
}
