<script setup lang="ts">
import type { ExternalId } from '~/models/domain/external-id';

defineProps<{
	externalIds: ExternalId[] | undefined
}>();
</script>
<template>
    <div
        v-if="(externalIds ?? []).length > 0"
        class="w-full overflow-x-scroll flex justify-start pb-5"
    >
        <span class="prose-lg">More info on</span>
        <NuxtLink
            v-for="externalLink in externalIds"
            :key="externalLink.value"
            :to="externalLink.url"
            class="badge badge-primary ml-2 mt-1.5"
        >
            {{ externalLink.provider_name }}
        </NuxtLink>
    </div>
</template>
