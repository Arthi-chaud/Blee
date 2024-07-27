<script setup lang="ts">
import type { Image as ImageType } from "~/models/domain/image";

defineProps<{
    resourceName: string | undefined;
    poster: ImageType | undefined | null;
}>();
</script>

<template>
    <div
        class="max-w-screen-md w-full flex flex-col md:flex-row gap-x-6"
        :style="{ aspectRatio: 16 / 9, height: 'fit-content' }"
    >
        <!--TODO Shift SSR/CSR-->
        <!--TODO Handle resposivity-->
        <div class="flex-1">
            <div class="h-full w-full flex items-center">
                <Image :image="poster" image-type="poster" />
            </div>
        </div>
        <div class="flex-2 flex flex-col justify-evenly">
            <p
                v-if="resourceName"
                class="prose-xl line-clamp-2 break-all w-full"
            >
                {{ resourceName }}
            </p>
            <div v-else class="skeleton h-5 w-20" />
            <div><slot name="secondary"></slot></div>
            <div><slot name="ternary"></slot></div>
        </div>
    </div>
</template>
