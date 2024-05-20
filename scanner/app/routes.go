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

type ScannerTask string

const (
	Idle  = "idle"
	Scan  = "scan"
	Clean = "clean"
)

type ScannerContext struct {
	currentTask ScannerTask
	config      *config.Config
}

func getFilesFromAPIOrFail(s *ScannerContext) ([]string, error) {
	knownPaths, err := api.GetAllKnownPaths(*s.config)
	if err != nil {
		return nil, echo.NewHTTPError(http.StatusServiceUnavailable, "Could not get info from API.")
	}
	return knownPaths, nil
}

func failIfNotIdle(s *ScannerContext) error {
	if s.currentTask != Idle {
		return echo.NewHTTPError(http.StatusLocked, "A task is already being processed. What for it to end.")
	}
	return nil
}

type ScannerStatus struct {
	Message string `json:"message"`
	Status  string `json:"status"`
}

func (s *ScannerContext) Status(c echo.Context) error {
	return c.JSON(http.StatusOK, ScannerStatus{Message: "Scanner is alive.", Status: string(s.currentTask)})
}

func (s *ScannerContext) Scan(c echo.Context) error {
	err := failIfNotIdle(s)
	if err != nil {
		return err
	}
	knownPaths, err := getFilesFromAPIOrFail(s)
	if err != nil {
		return err
	}
	s.currentTask = Scan
	go func() {
		for _, path := range pkg.GetWatchedFiles(s.config.WatchDir) {
			if !slices.Contains(knownPaths, path) && pkg.FileIsVideo(path) {
				actions.RegisterFile(path, s.config)
			}
		}
		s.currentTask = Idle
	}()
	return nil
}

func (s *ScannerContext) Clean(c echo.Context) error {
	err := failIfNotIdle(s)
	if err != nil {
		return err
	}
	knownPaths, err := getFilesFromAPIOrFail(s)
	if err != nil {
		return err
	}
	s.currentTask = Clean
	go func() {
		watchedFiles := pkg.GetWatchedFiles(s.config.WatchDir)
		for _, path := range knownPaths {
			if !slices.Contains(watchedFiles, path) && pkg.FileIsVideo(path) {
				actions.DeleteFile(path, s.config)
			}
		}
		s.currentTask = Idle
	}()
	return nil
}
