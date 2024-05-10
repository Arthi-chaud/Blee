package pkg

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/gabriel-vasile/mimetype"
)

func FileIsVideo(path string) bool {
	mime, err := mimetype.DetectFile(path)
	if err != nil {
		return false
	}
	return strings.HasPrefix(mime.String(), "video/")
}

func FileIsImage(path string) bool {
	mime, err := mimetype.DetectFile(path)
	if err != nil {
		return false
	}
	return strings.HasPrefix(mime.String(), "image/")
}

func ParseUint64(str string) uint64 {
	i, err := strconv.ParseUint(str, 10, 64)
	if err != nil {
		println(str)
		return 0
	}
	return i
}

func ParseFloat(str string) float32 {
	f, err := strconv.ParseFloat(str, 32)
	if err != nil {
		return 0
	}
	return float32(f)
}

func ParseTime(str string) float32 {
	x := strings.Split(str, ":")
	hours, minutes, sms := ParseFloat(x[0]), ParseFloat(x[1]), x[2]
	y := strings.Split(sms, ".")
	seconds, ms := ParseFloat(y[0]), ParseFloat(y[1])

	return (hours*60.+minutes)*60. + seconds + ms/1000.
}

// Applies `f` to each element of ts
func Map[T, U any](ts []T, f func(T, int) U) []U {
	us := make([]U, len(ts))
	for i := range ts {
		us[i] = f(ts[i], i)
	}
	return us
}

// https://stackoverflow.com/questions/37562873/most-idiomatic-way-to-select-elements-from-an-array-in-golang
func Filter[T any](ss []T, test func(T, int) bool) (ret []T) {
	for i, s := range ss {
		if test(s, i) {
			ret = append(ret, s)
		}
	}
	return
}

func GetPosterPathInFolder(path string) string {
	list, err := filepath.Glob(fmt.Sprintf("%s/*.*", path))

	if err != nil {
		return ""
	}
	for _, path := range list {
		if FileIsImage(path) {
			return path
		}
	}
	return ""
}
