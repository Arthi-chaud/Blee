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
const secondaryHref =
    props.secondaryHref === null
        ? undefined
        : (props.secondaryHref ??
          (props.package ? `/artists/${props.package?.artist_id}` : undefined));
</script>
<template>
    <div>
        <div
            class="aspect-[3/4] hover:scale-105 transition-transform duration-200"
        >
            <NuxtLink :to="packageLink">
                <Image :image="package?.poster" :expectedAspectRatio="3 / 4" />
            </NuxtLink>
        </div>
        <div class="flex flex-col pt-2 pl-2 text-sm">
            <NuxtLink :to="packageLink">
                <p class="line-clamp-1 hover:underline cursor-pointer">
                    {{ package?.name }}
                </p>
            </NuxtLink>
            <NuxtLink :to="secondaryHref">
                <p
                    class="line-clamp-1 font-light"
                    :class="{ 'hover:underline': secondaryHref != undefined }"
                >
                    {{
                        package
                            ? (formatSecondaryTitle?.(package) ??
                              package?.artist_name ??
                              "")
                            : ""
                    }}
                </p>
            </NuxtLink>
        </div>
    </div>
</template>
