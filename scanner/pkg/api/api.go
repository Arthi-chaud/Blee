package api

import (
	"fmt"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/go-playground/validator/v10"
	"github.com/goccy/go-json"
	"github.com/kpango/glg"
	"io"
	"net/http"
)

func HealthCheck(config config.Config) error {
	_, err := request("GET", "/", config)
	return err
}

func GetAllKnownPaths(config config.Config) ([]string, error) {
	next := "/files"
	filePaths := []string{}
	validate := validator.New(validator.WithRequiredStructEnabled())

	for len(next) != 0 {
		res, err := request("GET", next, config)
		if err != nil {
			return []string{}, err
		}
		var page Page[File] = Page[File]{}
		err = json.Unmarshal([]byte(res), &page)
		if err != nil {
			return []string{}, err
		}
		if err = validate.Struct(page); err != nil {
			return []string{}, err.(validator.ValidationErrors)
		}
		for _, item := range page.items {
			filePaths = append(filePaths, item.path)
		}
		next = page.metadata.next
	}
	return filePaths, nil
}

func request(method string, url string, config config.Config) (string, error) {
	client := &http.Client{}
	req, _ := http.NewRequest(method, fmt.Sprintf("%s%s", config.ApiUrl, url), nil)
	req.Header.Add("SCANNER_API_KEY", config.ApiKey)
	resp, err := client.Do(req)

	if err != nil {
		glg.Fatalln(err)
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode >= 400 {
		glg.Fatalln(err)
		return "", err
	}
	b, err := io.ReadAll(resp.Body)
	if err != nil {
		glg.Fatalln(err)
		return "", err
	}
	return string(b), nil
}
