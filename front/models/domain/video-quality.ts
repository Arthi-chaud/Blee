const VideoQualities = [
    "8k",
    "4k",
    "2k",
    "1080p",
    "720p",
    "576p",
    "480p",
    "360p",
    "240p",
    "other",
] as const;
type VideoQuality = (typeof VideoQualities)[number];

export { type VideoQuality, VideoQualities };
