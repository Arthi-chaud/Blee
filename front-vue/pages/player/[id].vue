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
const progress = ref(startTimestamp);
const videoRef = ref<HTMLVideoElement>();
const dataIsLoaded = ref(false);
watch([file, videoRef], ([f, player]) => {
    console.log('A')
    if (f && player && !player.src) {
        console.log('B')
        // player.src = "/transcoder/L3ZpZGVvcy9Db25jZXJ0cy9Nb2xva28gLSAxMSwwMDAgQ2xpY2tzL01vbG9rbyAtIDExLDAwMCBDbGlja3MubXA0/direct"
        // // const b64Path = Buffer.from(f.path).toString("base64url");
        // // streamUrl.value = `/transcoder/${b64Path}/direct`;
        // player.play().then(() => {
        //     dataIsLoaded.value = true;
        // });
    }
});
</script>
<template>
    <div class="w-full h-full relative">
        <!--TODO: Fade out when data is ready-->
        <div class="w-full h-full flex justify-center absolute">
            <!--Rework placement-->
            <Image :image="thumbnail" image-type="thumbnail" />
        </div>
        <div
            class="absolute w-full h-full top-0 bottom-0 z-10 bg-black opacity-60"
        ></div>
        <div
            class="absolute w-full h-top flex justify-center top-0 bottom-0 z-10"
        >
            <span class="loading loading-spinner loading-lg text-primary" />
        </div>
        <video ref="videoRef" class="w-full h-full absolute" />
        <!-- TODO Subtitle should include chapter if resouce is movie -->
        <!-- TODO Handle Progress -->
        <PlayerControls
            :chapters="chapters"
            :poster="parentPackage?.poster"
            :total-duration="file?.duration"
            :on-slide="(p) => (progress = p)"
            :progress="progress"
            :title="(movie ?? extra)?.name"
            :subtitle="(movie ?? extra)?.artist_name"
            :can-go-back="canGoBack"
            :on-back-button-tap="() => (canGoBack ? r.go(-1) : r.replace('/'))"
        />
    </div>
</template>
