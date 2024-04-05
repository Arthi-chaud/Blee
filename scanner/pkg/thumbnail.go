package pkg

import (
	"bytes"
	"fmt"
	// "fmt"
	"io"
	"os"

	// "time"

	"github.com/u2takey/ffmpeg-go"
)

func GetFrame(filepath string, timestamp int64) (io.Reader, error) {
	formattedDuration := fmt.Sprintf("%.2d:%.2d:%.2d", int(timestamp/3600), (timestamp/60)%60, timestamp%60)
	buf := bytes.NewBuffer(nil)
	err := ffmpeg_go.Input(filepath, ffmpeg_go.KwArgs{"ss": formattedDuration}).
		Filter("select", ffmpeg_go.Args{"gte(n,1)"}).
		Output("pipe:", ffmpeg_go.KwArgs{"vframes": 1, "format": "image2", "vcodec": "mjpeg"}).
		WithOutput(buf, os.Stdout).Run()
	return buf, err
}

