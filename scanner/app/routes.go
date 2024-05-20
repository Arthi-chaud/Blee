package main

import (
	"net/http"
	"slices"

	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/actions"
	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/labstack/echo/v4"
)

type ScannerContext struct {
	config *config.Config
}

func getFilesFromAPIOrFail(s *ScannerContext) ([]string, error) {
	knownPaths, err := api.GetAllKnownPaths(*s.config)
	if err != nil {
		return nil, echo.NewHTTPError(http.StatusServiceUnavailable, "Could not get info from API.")
	}
	return knownPaths, nil
}

func (s *ScannerContext) Scan(c echo.Context) error {
	knownPaths, err := getFilesFromAPIOrFail(s)
	if err != nil {
		return err
	}
	go func() {
		for _, path := range pkg.GetWatchedFiles(s.config.WatchDir) {
			if !slices.Contains(knownPaths, path) && pkg.FileIsVideo(path) {
				actions.RegisterFile(path, s.config)
			}
		}
	}()
	return nil
}

func (s *ScannerContext) Clean(c echo.Context) error {
	knownPaths, err := getFilesFromAPIOrFail(s)
	if err != nil {
		return err
	}
	go func() {
		watchedFiles := pkg.GetWatchedFiles(s.config.WatchDir)
		for _, path := range knownPaths {
			if !slices.Contains(watchedFiles, path) && pkg.FileIsVideo(path) {
				actions.DeleteFile(path, s.config)
			}
		}
	}()
	return nil
}
