package main

import (
	"flag"
	"log"
)

type Config struct {
	// The Directory to watch
	WatchDir string
}

// Parses and return a config from the CLI args
func get_config() Config {
	var config Config
	watchDir := flag.String("d", "", "the directory to watch")
	flag.Parse()

	config.WatchDir = *watchDir
	log.Println(config.WatchDir)

	return config
}
