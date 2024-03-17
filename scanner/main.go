package main

import (
	"fmt"
	"github.com/radovskyb/watcher"
	"log"
	"os"
	"time"
)

func main() {
	c := parse_config(os.Args)
	w := watcher.New()
	go func() {
		for {
			select {
			case event := <-w.Event:
				fmt.Println(event)
			case err := <-w.Error:
				log.Fatalln(err)
			case <-w.Closed:
				return
			}
		}
	}()
	// Watch this folder for changes.
	if err := w.AddRecursive(c.WatchDir); err != nil {
		log.Fatalln(err)
	}
	fmt.Println("Starting Scanner...")
	// Check for changes every 10s.
	if err := w.Start(time.Second * 10); err != nil {
		log.Fatalln(err)
	}
}
