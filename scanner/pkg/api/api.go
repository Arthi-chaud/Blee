package api

import (
	"bytes"
	"fmt"
	"io"
	"net/http"

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

func SaveMovie(movie *models.NewMovieDto, config config.Config) error {
	serialized, err := json.Marshal(movie)
	if err != nil {
		return err
	}
	_, err = request("POST", "/movies", bytes.NewBuffer(serialized), config)
	return err
}

func SaveExtra(movie *models.NewExtraDto, config config.Config) error {
	serialized, err := json.Marshal(movie)
	if err != nil {
		return err
	}
	_, err = request("POST", "/extras", bytes.NewBuffer(serialized), config)
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
