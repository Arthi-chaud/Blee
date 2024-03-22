package main

import (
	"flag"
	"github.com/kpango/glg"
	"os"
)

type Config struct {
	// The Directory to watch
	WatchDir string
	// Secret API Key to authenticate
	ApiKey string
}

// Parses and return a config from the CLI args and env args
func get_config() Config {
	var config Config
	watchDir := flag.String("d", "", "the directory to watch")
	flag.Parse()

	apiKey, is_present := os.LookupEnv("SCANNER_API_KEY")

	if !is_present || len(apiKey) == 0 {
		glg.Fatalf("SCANNER_API_KEY is missing or empty.")
		os.Exit(1)
	}

	config.ApiKey = apiKey
	config.WatchDir = *watchDir

	return config
}
