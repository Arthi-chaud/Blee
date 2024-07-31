<script setup lang="ts">
defineProps<{
    progress: number;
    totalDuration: number | undefined;
    onClick: (requestedProgress: number) => void;
}>();
</script>
<template>
    <div class="flex items-center">
        <input
            type="range"
            min="0"
            :max="totalDuration ?? 1"
            :value="progress ?? 0"
            class="player-slider"
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
    </div>
</template>
<style scoped>
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
        @apply rounded-box bg-base-content/10 h-1.5 w-full;
    }
    &::-moz-range-track {
        @apply rounded-box bg-base-content/10 h-1.5 w-full;
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
