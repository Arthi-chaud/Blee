<script setup lang="ts">
import { API } from "~/api/api";
import Image from "~/components/image.vue";
import { useResourceMetadata } from "~/composables/player/resource-metadata";

const ParamSeparator = ":";
type ValidParamPrefix = "extra" | "movie";
definePageMeta({
    layout: "empty",
    validate: async (route) => {
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
const videoRef = ref<HTMLVideoElement>();
const progress = ref(videoRef.value?.currentTime ?? startTimestamp);
const bufferedTime = ref(0);
const bufferClock = ref<NodeJS.Timeout>();
const dataIsLoaded = ref(false);
const goBack = () => {
    canGoBack ? r.go(-1) : r.replace("/");
};
const updateBufferedTime = () => {
    if (videoRef.value) {
        const lastBufferedSegment = videoRef.value.buffered.length - 1;
        if (lastBufferedSegment < 1) {
            return;
        }
        bufferedTime.value = videoRef.value.buffered.end(lastBufferedSegment);
    }
};
2;
watch(
    [file, videoRef],
    ([f, player]) => {
        if (f && player && !player.src) {
            player.playsInline = true;
            const transcoderUrl = API.buildDirectPlaybackUrl(f);
            player.src = transcoderUrl;
            player.currentTime = startTimestamp;
            player.play().then(() => {
                dataIsLoaded.value = true;
                player.ontimeupdate = () => {
                    progress.value = Math.floor(player.currentTime);
                };
                player.onended = goBack;
            });
            bufferClock.value = setInterval(updateBufferedTime, 300);
        }
    },
    { immediate: true },
);
onBeforeUnmount(() => {
    videoRef?.value?.pause();
    clearTimeout(bufferClock.value);
});
</script>
<template>
    <div class="w-full h-full relative">
        <!--TODO: Fade out when data is ready-->
        <div
            class="w-full h-full flex justify-center absolute loading-state-content"
            :style="{
                opacity: dataIsLoaded ? 0 : 1,
            }"
        >
            <!--Rework placement-->
            <Image :image="thumbnail" image-type="thumbnail" />
        </div>
        <div
            class="absolute w-full h-full top-0 bottom-0 z-10 bg-black opacity-60 loading-state-content"
            :style="{
                opacity: dataIsLoaded ? 0 : 1,
            }"
        ></div>
        <div
            class="absolute w-full h-top flex justify-center top-0 bottom-0 z-10 loading-state-content"
            :style="{
                opacity: dataIsLoaded ? 0 : 1,
            }"
        >
            <span class="loading loading-spinner loading-lg text-primary" />
        </div>
        <video ref="videoRef" class="w-full h-full absolute" />
        <!-- TODO Subtitle should include chapter if resouce is movie -->
        <!-- TODO Handle Progress -->
        <PlayerControls
            :buffered="bufferedTime"
            :chapters="chapters"
            :poster="parentPackage?.poster"
            :total-duration="file?.duration"
            :on-slide="
                (p) => {
                    if (videoRef) {
                        videoRef.currentTime = Math.floor(p);
                    }
                }
            "
            :progress="progress"
            :title="(movie ?? extra)?.name"
            :subtitle="(movie ?? extra)?.artist_name"
            :can-go-back="canGoBack"
            :on-back-button-tap="goBack"
        />
    </div>
</template>
<style>
.loading-state-content {
    transition: "opacity 0.2s ease-in";
}
</style>
