package main

import (
	"os"

	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
	"github.com/labstack/echo/v4"
)

func setupLogger() {
	glg.Get().
		SetMode(glg.STD).
		// We will be watching the logs through docker-compose
		// It already provides timestamps
		DisableTimestamp().
		SetLineTraceMode(glg.TraceLineNone)
}

func main() {
	setupLogger()
	c := config.GetConfig()

	if err := api.HealthCheck(c); err != nil {
		glg.Fatalf("Failed connecting to API: %s", err)
		os.Exit(1)
	}
	e := echo.New()

	s := ScannerContext {
		config: &c,
	}

	e.POST("/scan", s.Scan)
	e.POST("/clean", s.Clean)

	// knownPaths, err := api.GetAllKnownPaths(c)
	// if err != nil {
	// 	glg.Fatalf("Could not get registered files from API: %s", err)
	// 	os.Exit(1)
	// }
	// for _, path := range watchedFiles {
	// 	if !slices.Contains(knownPaths, path) && pkg.FileIsVideo(path) {
	// 		actions.RegisterFile(path, &c)
	// 	}
	// }
	// for _, path := range knownPaths {
	// 	if !slices.Contains(watchedFiles, path) && pkg.FileIsVideo(path) {
	// 		actions.DeleteFile(path, &c)
	// 	}
	// }
	glg.Log("Scanner started! Let's get this show on the road.")
	e.Logger.Fatal(e.Start(":8133"))
}
