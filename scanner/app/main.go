package main

import (
	"fmt"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
	"github.com/radovskyb/watcher"
	"os"
	"time"
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
	w := watcher.New()
	go func() {
		for {
			select {
			case event := <-w.Event:
				fmt.Println(event)
			case err := <-w.Error:
				glg.Fatalf("File System Watcher errored: %s", err)
				os.Exit(1)
			case <-w.Closed:
				return
			}
		}
	}()
	glg.Logf("Attempting to watch %s", c.WatchDir)
	// Watch this folder for changes.
	if err := w.AddRecursive(c.WatchDir); err != nil {
		glg.Fatalf("Could not watch: %s", err)
		os.Exit(1)
	}
	glg.Log("Scanner started! Let's get this show on the road.")
	// Check for changes every 10s.
	if err := w.Start(time.Second * 10); err != nil {
		glg.Fatalf("Starting Polling Cycle errored: %s", err)
		os.Exit(1)
	}
}
