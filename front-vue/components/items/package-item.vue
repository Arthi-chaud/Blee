<script setup lang="ts">
import type { Package } from "~/models/domain/package";

const props = defineProps<{
    package: Package | undefined;
    formatSecondaryTitle?: (p: Package) => string;
}>();
const packageLink = ref(
    props.package ? `/packages/${props.package?.id}` : null,
);
</script>
<template>
    <div>
        <div class="aspect-[3/4]">
            <Image
                :image="package?.poster"
                :expectedAspectRatio="3 / 4"
            />
        </div>
        <div class="flex flex-col pt-2 pl-2 text-sm">
            <p class="line-clamp-1">{{ package?.name }}</p>
            <p class="line-clamp-1 font-light">
                {{
                    package
                        ? (formatSecondaryTitle?.(package) ?? package?.artist_name ?? '')
                        : ''
                }}
            </p>
        </div>
    </div>
</template>
