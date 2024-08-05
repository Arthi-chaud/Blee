import { API } from "~/api/api";
import type { File } from "~/models/domain/file";

export const usePlayerContext = (props: {
    fileData: Ref<File> | Ref<undefined>;
    onEnd: () => void;
    onReady: () => void;
    startTimestamp: number | undefined;
}) => {
    const progress = ref(props.startTimestamp ?? 0);
    const videoRef = ref<HTMLVideoElement>();
    const bufferedTime = ref(0);
    const bufferClock = ref<NodeJS.Timeout>();
    const playerIsReady = ref(false);
    const isPlaying = ref(false);
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
        [props.fileData, videoRef],
        ([f, player]) => {
            if (!f || !player || player.src) {
                return;
            }
            player.playsInline = true;
            const playbackUrl = API.buildDirectPlaybackUrl(f);
            player.src = playbackUrl;
            player.currentTime = progress.value;
            isPlaying.value = true;
            player
                .play()
                .then(() => {
                    playerIsReady.value = true;
                    props.onReady();
                    player.onerror = console.error;
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
                })
                .catch(console.error);
            bufferClock.value = setInterval(updateBufferedTime, 500);
        },
        { immediate: true },
    );
    onBeforeUnmount(() => {
        videoRef?.value?.pause();
        clearTimeout(bufferClock.value);
    });
    return {
        playerRef: videoRef,
        progress: progress,
        isReady: playerIsReady,
        isPlaying: isPlaying,
        buffered: bufferedTime,
    };
};
