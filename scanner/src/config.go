package src

import (
	"flag"
	"github.com/goccy/go-json"
	"github.com/kpango/glg"
	"gopkg.in/go-playground/validator.v9"
	"os"
)

type Config struct {
	// The Directory to watch
	WatchDir string
	// Secret API Key to authenticate
	ApiKey string
	// User configuration from `scanner.json`
	UserConfig UserConfiguration
}

type UserConfiguration struct {
	Regexes struct {
		// Regexes of the extras to match
		Extra []string `json:"extras" validate:"required"`
		// Regexes of the movies to match
		Movie []string `json:"movies" validate:"required"`
	} `json:"regexes" validate:"required,dive,required"`
}

func parseConfigFile(file string) UserConfiguration {
	bytes, err := os.ReadFile(file)

	if err != nil {
		glg.Fatalf("Could not read configuration file: %s", err)
		os.Exit(1)
	}

	var config UserConfiguration

	json.Unmarshal(bytes, &config)
	validation_error := validator.New().Struct(config)
	if validation_error != nil {
		glg.Fatalf("An error occured while validating configuration file: %s", validation_error)
		os.Exit(1)
	}
	return config
}

// Parses and return a config from the CLI args and env args
func GetConfig() Config {
	var config Config
	watchDir := flag.String("d", "", "the directory to watch")
	configFilePath := flag.String("c", "", "the path to the `scanner.json`")
	flag.Parse()

	if len(*watchDir) == 0 || len(*configFilePath) == 0 {
		glg.Fatalf("Missing argument. Run with `-h` for usage.")
		os.Exit(1)
	}

	apiKey, is_present := os.LookupEnv("SCANNER_API_KEY")

	if !is_present || len(apiKey) == 0 {
		glg.Fatalf("SCANNER_API_KEY is missing or empty.")
		os.Exit(1)
	}

	config.ApiKey = apiKey
	config.WatchDir = *watchDir
	config.UserConfig = parseConfigFile(*configFilePath)
	glg.Log("Configuration parsed successfully")
	return config
}
