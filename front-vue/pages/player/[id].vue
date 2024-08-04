<script setup lang="ts">
import Image from "~/components/image.vue";
import { useBrowserMetadata } from "~/composables/player/browser-metadata";
import { usePlayerContext } from "~/composables/player/context";
import { useCurrentChapter } from "~/composables/player/current-chapter";
import { useResourceMetadata } from "~/composables/player/resource-metadata";

const ParamSeparator = ":";
type ValidParamPrefix = "extra" | "movie";
definePageMeta({
    layout: "empty",
    validate: (route) => {
        const param = route.params.id;
        if (typeof param !== "string") {
            return false;
        }
        return param.startsWith("extra:") || param.startsWith("movie:");
    },
});
const route = useRoute();
const { resourceType, resourceId, startTimestamp } = (() => {
    const rawParam = route.params.id as string;
    const splitParam = rawParam.split(ParamSeparator, 2);
    const resourceType: ValidParamPrefix =
        splitParam[0] === "extra" ? "extra" : "movie";
    const resourceId = splitParam[1];
    const rawStartTimestamp = route.query.start as string | undefined;
    const startTimestamp = rawStartTimestamp ? parseInt(rawStartTimestamp) : 0;
    return { resourceType, resourceId, startTimestamp };
})();
const { file, parentPackage, movie, extra, thumbnail, chapters } =
    useResourceMetadata(resourceType, resourceId);
const r = useRouter();
const canGoBack = r.options.history.state["back"] != null;
const goBack = () => {
    canGoBack ? r.go(-1) : r.replace("/");
};
const setupControls = () => {
    if (typeof navigator.mediaSession !== "undefined") {
        navigator.mediaSession.setActionHandler("play", () =>
            playerRef.value?.play(),
        );
        navigator.mediaSession.setActionHandler("pause", () =>
            playerRef.value?.pause(),
        );
        // TODO: use back + forward button to change chapter
    }
};
const { isReady, buffered, isPlaying, playerRef, progress } = usePlayerContext({
    startTimestamp,
    fileData: file,
    onEnd: goBack,
    onReady: () => {
        setupControls();
        setMediaMetadata();
    },
});
const { currentChapter } = useCurrentChapter({
    chapters: chapters,
    onChapterChange: () => setMediaMetadata(),
    progress: progress,
});
const { setMediaMetadata } = useBrowserMetadata({
    currentChapter,
    movie,
    extra,
    parentPackage,
});
</script>
<template>
    <div class="w-full h-full relative">
        <video
            ref="playerRef"
            class="w-full h-full absolute opacity-0 fading-content"
            :class="{ 'opacity-100': isReady }"
        />
        <div
            class="w-full h-full flex justify-center absolute fading-content"
            :class="{ 'opacity-0': isReady }"
        >
            <Image :image="thumbnail" image-type="thumbnail" />
            <!--Rework placement-->
        </div>
        <div
            class="absolute w-full h-full top-0 bottom-0 z-10 bg-black opacity-60 fading-content"
            :class="{ '!opacity-0': isReady }"
        ></div>
        <div
            class="absolute w-full h-top flex justify-center top-0 bottom-0 z-10 fading-content"
            :class="{ 'opacity-0': isReady }"
        >
            <span class="loading loading-spinner loading-lg text-primary" />
        </div>
        <PlayerControls
            :buffered="buffered"
            :chapters="chapters"
            :poster="parentPackage?.poster"
            :total-duration="file?.duration"
            :is-playing="isPlaying"
            :on-play="
                (wantToPlay) => {
                    if (wantToPlay) {
                        playerRef?.play();
                    } else {
                        playerRef?.pause();
                    }
                }
            "
            :on-slide="
                (p) => {
                    if (playerRef) {
                        playerRef.currentTime = Math.floor(p);
                    }
                }
            "
            :progress="progress"
            :title="(movie ?? extra)?.name"
            :subtitle="
                currentChapter
                    ? `${currentChapter.name} - ${(movie ?? extra)?.artist_name}`
                    : (movie ?? extra)?.artist_name
            "
            :can-go-back="canGoBack"
            :on-back-button-tap="goBack"
        />
    </div>
</template>
<style scoped>
.fading-content {
    transition: opacity 0.2s ease-in;
}
</style>
