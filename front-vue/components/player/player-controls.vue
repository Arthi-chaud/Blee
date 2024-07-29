<script setup lang="ts">
defineProps<{
    canGoBack: boolean;
    onBackButtonTap: () => void;
}>();
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
        <div class="w-full absolute bottom-0 h-10 bg-red-300"></div>
    </div>
</template>
