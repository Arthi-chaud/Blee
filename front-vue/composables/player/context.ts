import Hls from "hls.js";
import { API } from "~/api/api";
import type { File } from "~/models/domain/file";
import type { FileInfo } from "~/models/domain/file-info";

// Stolen from Kyoo
const canBePlayedNatively = (info: FileInfo, videoRef: HTMLVideoElement) => {
    let codec = info.mimeCodec;
    if (!codec) {
        return false;
    }
    if (navigator.userAgent.search("Firefox") === -1)
        codec = codec.replace("video/x-matroska", "video/mp4");
    return !!videoRef.canPlayType(codec);
};

function uuidv4(): string {
    // @ts-expect-error I have no clue how this works, thanks https://stackoverflow.com/questions/105034/how-do-i-create-a-guid-uuid
    return ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, (c) =>
        (
            c ^
            (crypto.getRandomValues(new Uint8Array(1))[0] & (15 >> (c / 4)))
        ).toString(16),
    );
}

const transcoderClientId = isSSR() ? "" : uuidv4();

export const usePlayerContext = (props: {
    fileData: Ref<File> | Ref<undefined>;
    fileInfoData: Ref<FileInfo> | Ref<undefined>;
    onEnd: () => void;
    onReady: () => void;
    startTimestamp: number | undefined;
}) => {
    const progress = ref(props.startTimestamp ?? 0);
    const videoRef = ref<HTMLVideoElement>();
    const playMode = ref<"direct" | "hls">("direct");
    const bufferedTime = ref(0);
    const bufferClock = ref<NodeJS.Timeout>();
    const playerIsReady = ref(false);
    const isPlaying = ref(false);
    const hlsRef = ref<Hls>();
    const setupHls = () => {
        const hls = hlsRef.value;
        if (hls) {
            hls.destroy();
        }
        hlsRef.value = new Hls({
            xhrSetup: async (xhr) => {
                xhr.setRequestHeader("X-CLIENT-ID", transcoderClientId);
            },
        });
    };
    const updateBufferedTime = () => {
        if (videoRef.value) {
            const lastBufferedSegment = videoRef.value.buffered.length - 1;
            if (lastBufferedSegment < 1) {
                return;
            }
            bufferedTime.value =
                videoRef.value.buffered.end(lastBufferedSegment);
        }
    };
    watch(
        [props.fileData, props.fileInfoData, videoRef, playMode],
        ([f, i, player, mode]) => {
            if (!f || !i || !player || player.src) {
                return;
            }
            const onPlaybackStart = () => {
                playerIsReady.value = true;
                props.onReady();
                player.onerror = (_, __, ___, ____, error) => {
                    if (mode === "hls") {
                        //TODO
                        console.error(error);
                        return;
                    }
                    if (
                        player.error?.code ==
                        MediaError.MEDIA_ERR_SRC_NOT_SUPPORTED
                    ) {
                        console.log("Moving to transcoding");
                        playMode.value = "hls";
                    }
                    console.error(error);
                };
                player.ontimeupdate = () => {
                    progress.value = Math.floor(player.currentTime);
                };
                player.onpause = () => {
                    isPlaying.value = false;
                };
                player.onplay = () => {
                    isPlaying.value = true;
                };
                player.onended = props.onEnd;
            };
            const playDirectVideo = () => {
                console.log(canBePlayedNatively(i, player))
                if (!canBePlayedNatively(i, player)) {
                    playMode.value = "hls";
                    playHls();
                } else {
                    const playbackUrl = API.buildDirectPlaybackUrl(f);
                    player.src = playbackUrl;
                    player.play().then(onPlaybackStart);
                }
            };

            const playHls = () => {
                setupHls();
                const playbackUrl = API.buildTranscodedPlaybackUrl(f);
                hlsRef.value!.loadSource(playbackUrl);
                hlsRef.value!.attachMedia(player);
                hlsRef.value!.startLoad(progress.value);
                player.play().then(onPlaybackStart);
                hlsRef.value!.on(Hls.Events.ERROR, (_, d) => {
                    if (!d.fatal || !hlsRef.value?.media) {
                        return;
                    }
                    console.error("Hls error", d);
                });
            };
            player.playsInline = true;
            player.currentTime = progress.value;
            isPlaying.value = true;
            playDirectVideo();
            if (bufferClock.value) {
                clearInterval(bufferClock.value);
            }
            bufferClock.value = setInterval(updateBufferedTime, 500);
        },
        { immediate: true },
    );
    onBeforeUnmount(() => {
        videoRef?.value?.pause();
        hlsRef.value?.destroy();
        clearInterval(bufferClock.value);
    });
    return {
        playerRef: videoRef,
        progress: progress,
        isReady: playerIsReady,
        isPlaying: isPlaying,
        buffered: bufferedTime,
    };
};
