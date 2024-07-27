<script setup lang="ts">
import type { Image, ImageType } from "~/models/domain/image";

const { imageType } = defineProps<{
    title: string | undefined;
    href: string | null;
    image: Image | null | undefined;
    imageType: ImageType;
    fitImageToPlaceholderRatio?: true;
    secondaryTitle: string | undefined | null;
    secondaryHref: string | null;
}>();
const aspectRatio = imageType == "poster" ? 2 / 3 : 16 / 9;
</script>
<template>
    <div
        class="flex flex-col h-full justify-end"
        :style="{ justifyContent: image ? 'end' : 'start' }"
    >
        <div
            class="flex items-end hover:scale-105 transition-transform duration-200"
            :style="{
                objectFit: 'contain',
                overflow: 'hidden',
                aspectRatio: aspectRatio
            }"
        >
            <NuxtLink
                :to="href ?? undefined"
                class="h-full w-full flex items-end"
            >
                <Image
                    :image="image"
                    :image-type="imageType"
                    :fit-to-expected-aspect-ratio="fitImageToPlaceholderRatio"
                />
            </NuxtLink>
        </div>
        <div
            class="flex flex-col pt-2 text-sm"
            :class="{ 'pl-2': title !== undefined }"
        >
            <Transition>
                <NuxtLink v-if="title" :to="href ?? undefined" class="w-full">
                    <p
                        class="line-clamp-1 break-all hover:underline cursor-pointer w-full"
                    >
                        {{ title }}
                    </p>
                </NuxtLink>
                <p
                    v-else-if="title === undefined"
                    class="skeleton h-4 w-full mb-1.5 mt-0.5"
                ></p>
            </Transition>
            <Transition>
                <NuxtLink
                    v-if="secondaryTitle"
                    :to="secondaryHref ?? undefined"
                    class="w-full"
                >
                    <p
                        class="line-clamp-1 font-light break-all w-full"
                        :class="{
                            'hover:underline': secondaryHref != undefined,
                        }"
                    >
                        {{ secondaryTitle }}
                    </p>
                </NuxtLink>
                <p
                    v-else-if="secondaryTitle === undefined"
                    class="skeleton h-3.5 w-half"
                ></p>
            </Transition>
        </div>
    </div>
</template>
