<script setup lang="ts">
import type { Package } from "~/models/domain/package";

const props = defineProps<{
    package: Package | undefined;
    secondaryHref?: string | null;
    formatSecondaryTitle?: (p: Package) => string;
}>();
const packageLink = ref(
    props.package ? `/packages/${props.package?.id}` : undefined,
);
const secondaryHref = computed(() =>
    props.secondaryHref === null
        ? undefined
        : (props.secondaryHref ??
          (props.package ? `/artists/${props.package?.artist_id}` : undefined)),
);
const secondaryTitle = computed(() =>
    props.package
        ? (props.formatSecondaryTitle?.(props.package) ??
          props.package?.artist_name ??
          undefined)
        : undefined,
);
</script>
<template>
    <div
        class="flex flex-col h-full justify-end"
        :style="{ justifyContent: package ? 'end' : 'start' }"
    >
        <div
            class="aspect-[2/3] flex items-end hover:scale-105 transition-transform duration-200"
        >
            <NuxtLink :to="packageLink" class="h-full w-full flex items-end">
                <Image :image="package?.poster" :expectedAspectRatio="2 / 3" />
            </NuxtLink>
        </div>
        <div
            class="flex flex-col pt-2 text-sm"
            :class="{ 'pl-2': package != undefined }"
        >
            <NuxtLink :to="packageLink">
                <Transition>
                    <p
                        v-if="package"
                        class="line-clamp-1 break-all hover:underline cursor-pointer"
                    >
                        {{ package?.name }}
                    </p>
                    <p v-else class="skeleton h-4 w-full mb-1.5 mt-0.5"></p>
                </Transition>
            </NuxtLink>
            <NuxtLink :to="secondaryHref">
                <Transition>
                    <p
                        v-if="secondaryTitle"
                        class="line-clamp-1 font-light break-all"
                        :class="{
                            'hover:underline': secondaryHref != undefined,
                        }"
                    >
                        {{ secondaryTitle }}
                    </p>
                    <p v-else class="skeleton h-3.5 w-half"></p>
                </Transition>
            </NuxtLink>
        </div>
    </div>
</template>
