<script setup lang="ts">
import type { Chapter } from "~/models/domain/chapter";
import type { Image as ImageModel } from "~/models/domain/image";

const props = defineProps<{
    canGoBack: boolean;
    poster: ImageModel | undefined | null;
    title: string | undefined;
    subtitle: string | undefined | null;
    progress: number | undefined;
    chapters: Chapter[] | undefined;
    totalDuration: number | undefined;
    onSlide: (newProgess: number) => void;
    onBackButtonTap: () => void;
}>();
const chapterMarks = computed(
    () =>
        props.chapters?.map((c) => ({
            start: c.start_time,
            name: c.name,
            end: c.end_time,
        })) ?? [],
);
</script>
<template>
    <div class="w-full h-full z-20 fixed">
        <div class="top-0 h-20 flex justify-start m-1">
            <button class="btn btn-ghost" @click="onBackButtonTap()">
                <fa
                    :icon="canGoBack ? 'arrow-left' : 'home'"
                    class="prose-lg text-white"
                />
            </button>
        </div>
        <div
            class="w-full absolute bottom-0 h-[150px] controls flex p-3 flex-row"
        >
            <Image :image="poster" image-type="poster" />
            <div class="w-full flex flex-col justify-between mt-7 pl-3">
                <div class="pb-2">
                    <p
                        v-if="title !== undefined"
                        class="controls-text prose-lg"
                    >
                        {{ title }}
                    </p>
                    <div v-else class="skeleton h-4 w-20 my-2" />
                </div>
                <div class="pb-2">
                    <p
                        v-if="subtitle !== undefined"
                        class="controls-text prose-md"
                    >
                        {{ subtitle }}
                    </p>
                    <div v-else class="skeleton h-3 w-20" />
                </div>
                <div class="flex flex-row w-full items-center">
                    <p class="flex-none controls-text prose-sm">
                        {{
                            progress === undefined
                                ? "--:--"
                                : formatDuration(progress).padStart(
                                      totalDuration
                                          ? formatDuration(totalDuration).length
                                          : 5,
                                      "&nbsp;",
                                  )
                        }}
                    </p>
                    <div class="w-full flex-1 mx-2">
                        <PlayerSlider
                            :chapter-marks="chapterMarks"
                            :progress="progress ?? 0"
                            :total-duration="totalDuration"
                            :on-click="onSlide"
                        />
                    </div>
                    <div class="flex-none controls-text prose-sm">
                        <p v-if="totalDuration === undefined">--:--</p>
                        <p v-else>{{ formatDuration(totalDuration) }}</p>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>
<style>
.controls-text {
    color: white;
}
.controls {
    background-image: linear-gradient(transparent, black);
}
</style>
