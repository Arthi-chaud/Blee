<script setup lang="ts">
import type { Image } from "~/models/domain/image";

defineProps<{
    title: string | undefined;
    href: string | undefined;
    image: Image | null | undefined;
    secondaryTitle: string | undefined | null;
    secondaryHref?: string | null;
}>();
</script>
<template>
    <div
        class="flex flex-col h-full justify-end"
        :style="{ justifyContent: image ? 'end' : 'start' }"
    >
        <div
            class="aspect-[2/3] flex items-end hover:scale-105 transition-transform duration-200"
        >
            <NuxtLink
                :to="href"
                class="h-full w-full flex items-end"
            >
                <Image :image="image" :expectedAspectRatio="2 / 3" />
            </NuxtLink>
        </div>
        <div
            class="flex flex-col pt-2 text-sm"
            :class="{ 'pl-2': title !== undefined }"
        >
            <Transition>
                <NuxtLink v-if="title" :to="href ?? undefined">
                    <p
                        class="line-clamp-1 break-all hover:underline cursor-pointer"
                    >
                        {{ title }}
                    </p>
                </NuxtLink>
                <p v-else-if="title === undefined" class="skeleton h-4 w-full mb-1.5 mt-0.5"></p>
            </Transition>
            <Transition>
                <NuxtLink
                    v-if="secondaryTitle !== undefined"
                    :to="secondaryHref ?? undefined"
                >
                    <p
                        class="line-clamp-1 font-light break-all"
                        :class="{
                            'hover:underline': secondaryHref != undefined,
                        }"
                    >
                        {{ secondaryTitle }}
                    </p>
                </NuxtLink>
                <p v-else class="skeleton h-3.5 w-half"></p>
            </Transition>
        </div>
    </div>
</template>
