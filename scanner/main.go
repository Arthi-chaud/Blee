package main

import (
	"fmt"
	"github.com/radovskyb/watcher"
	"log"
	"time"
)

func main() {
	c := get_config()
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
	log.Println("Attempting to watch", c.WatchDir)
	// Watch this folder for changes.
	if err := w.AddRecursive(c.WatchDir); err != nil {
		log.Fatalln(err)
	}
	log.Println("Scanner started! Let's get this show on the road.")
	// Check for changes every 10s.
	if err := w.Start(time.Second * 10); err != nil {
		log.Fatalln(err)
	}
}
