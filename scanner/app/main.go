package main

import (
	"os"

	_ "github.com/Arthi-chaud/Blee/scanner/app/docs"
	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
	"github.com/labstack/echo/v4"
	"github.com/swaggo/echo-swagger"
)

func setupLogger() {
	glg.Get().
		SetMode(glg.STD).
		// We will be watching the logs through docker-compose
		// It already provides timestamps
		DisableTimestamp().
		SetLineTraceMode(glg.TraceLineNone)
}

// @title Blee's Scanner API
// @version 1.0
// @description The scanner is responsible for file parsing and registration.
func main() {
	setupLogger()
	c := config.GetConfig()

	if err := api.HealthCheck(c); err != nil {
		glg.Fatalf("Failed connecting to API: %s", err)
		os.Exit(1)
	}
	e := echo.New()

	s := ScannerContext{
		config:      &c,
		currentTask: Idle,
	}

	e.GET("/status", s.Status)
	e.POST("/scan", s.Scan)
	e.POST("/clean", s.Clean)
	e.GET("/swagger/*", echoSwagger.WrapHandler)

	glg.Log("Scanner started! Let's get this show on the road.")
	e.Logger.Fatal(e.Start(":8133"))
}
