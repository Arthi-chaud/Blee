package main

import (
	"log"
	"nullprogram.com/x/optparse"
)

type Config struct {
	// The Directory to watch
	WatchDir string
}

// Parses and return a config from the CLI args
func parse_config(args []string) Config {
	options := []optparse.Option{
		{Long: "dir", Short: 'd', Kind: optparse.KindRequired},
	}
	var config Config

	results, _, err := optparse.Parse(options, args)
	if err != nil {
		log.Fatalln(err)
	}

	for _, result := range results {
		switch result.Long {
		case "dir":
			config.WatchDir = result.Optarg
		}
	}
	if config.WatchDir == "" {
		log.Fatalln("Missing --dir option")
	}
	return config
}
