<!-- eslint-disable vue/multi-word-component-names -->
<script setup lang="ts">
import { faStar } from "@fortawesome/free-regular-svg-icons";
const { rating } = defineProps<{
    rating: number;
}>();
const getStarIconFromIndex = (starIndex: number) => {
    const normalizedRatingThreshold = (starIndex - 1) * 20;
    const normalizedNextRatingThreshold = starIndex * 20;
    if (rating <= normalizedRatingThreshold) {
        return faStar;
    } else if (
        normalizedRatingThreshold <= rating &&
        rating < normalizedNextRatingThreshold
    ) {
        return "star-half-stroke";
    }
    return "star";
};
</script>
<template>
    <div class="flex flex-row gap-x-0.5" :style="{ paddingTop: '0.2rem' }">
        <fa
            v-for="index in [1, 2, 3, 4, 5]"
            :key="`start-${index}`"
            :icon="getStarIconFromIndex(index)"
            class="prose-sm"
        />
    </div>
</template>
