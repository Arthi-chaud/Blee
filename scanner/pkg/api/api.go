package api

import (
	"fmt"
	"io"
	"net/http"

	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
)

func HealthCheck(config config.Config) error {
	_, err := request("GET", "/", config)
	return err
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
