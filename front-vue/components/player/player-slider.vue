<script setup lang="ts">
const props = defineProps<{
    progress: number;
    buffered: number;
    chapterMarks: { start: number; end: number; name: string }[];
    totalDuration: number | undefined;
    onClick: (requestedProgress: number) => void;
}>();
const bufferedWidth = ref(50);
watch(props, ({ totalDuration, buffered }) => {
    bufferedWidth.value = Math.floor(
        totalDuration ? (100 * buffered) / totalDuration : 0,
    );
});
const computeChapterMarkwidth = computed(
    () => (chapter: { start: number; end: number }) => {
        if (props.totalDuration === undefined) {
            return 0;
        }
        return Math.floor(
            (100 * (chapter.end - chapter.start)) / props.totalDuration,
        );
    },
);
//TODO: Make chapter mark use actual position of the range, not progress
</script>
<template>
    <div class="flex items-center relative">
        <input
            type="range"
            min="0"
            :max="totalDuration ?? 1"
            :value="progress ?? 0"
            class="player-slider absolute w-full"
            @change="
                (e) => {
                    if (totalDuration !== undefined) {
                        const target: any = e.target;
                        const newProgress = target.value;
                        // newProgress is a string :(
                        onClick(parseInt(newProgress));
                    }
                }
            "
        />
        <div
            class="absolute slider-height w-full rounded-box overflow-hidden pointer-events-none"
        >
            <!--Buffer is broken??-->
            <div
                class="bg-primary h-full"
                :style="{
                    borderTopRightRadius: 0,
                    borderBottomRightRadius: 0,
                    opacity: 0.5,
                    width: `${bufferedWidth}%`,
                }"
            />
        </div>
        <div
            v-if="totalDuration !== undefined"
            class="absolute slider-height w-full flex flex-row rounded-box overflow-hidden pointer-events-none"
        >
            <div
                v-for="(chapter, chapterIndex) in chapterMarks"
                :key="chapter.start"
                :style="{ width: `${computeChapterMarkwidth(chapter)}%` }"
                :class="{
                    ['border-l-2']: chapterIndex != 0,
                }"
                class="w-1 slider-height border-l-gray-400"
            />
        </div>
    </div>
</template>
<style scoped>
.slider-height {
    @apply h-1.5;
}
/* Stolen from DaisyUI + changed to our needs */
.player-slider {
    cursor: pointer;
    appearance: none;
    -webkit-appearance: none;
    --range-shdw: theme(colors.primary);
    @apply rounded-box w-full overflow-hidden bg-neutral;
    &:focus-visible::-webkit-slider-thumb {
        --focus-shadow: 0 0 0 6px theme(colors.base-100) inset,
            0 0 0 2rem var(--range-shdw) inset;
    }
    &:focus-visible::-moz-range-thumb {
        --focus-shadow: 0 0 0 6px theme(colors.base-100) inset,
            0 0 0 2rem var(--range-shdw) inset;
    }
    &::-webkit-slider-runnable-track {
        @apply rounded-box bg-base-content/10 slider-height w-full;
    }
    &::-moz-range-track {
        @apply rounded-box bg-base-content/10 slider-height w-full;
    }
    &::-webkit-slider-thumb {
        @apply rounded-box bg-base-100 relative h-0 w-0 border-none;
        appearance: none;
        -webkit-appearance: none;
        color: var(--range-shdw);
        transform: translateY(-50%);
        --filler-size: 100rem;
        --filler-offset: 0rem;
        box-shadow:
            0 0 0 3px var(--range-shdw) inset,
            var(--focus-shadow, 0 0),
            calc(var(--filler-size) * -1 - var(--filler-offset)) 0 0
                var(--filler-size);
    }
    &::-moz-range-thumb {
        @apply rounded-box bg-base-100 relative h-0 w-0 border-none;
        color: var(--range-shdw);
        --filler-size: 100rem;
        --filler-offset: 0rem;
        box-shadow:
            0 0 0 3px var(--range-shdw) inset,
            var(--focus-shadow, 0 0),
            calc(var(--filler-size) * -1 - var(--filler-offset)) 0 0
                var(--filler-size);
    }
}
</style>
