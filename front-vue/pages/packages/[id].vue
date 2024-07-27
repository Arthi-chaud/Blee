<script setup lang="ts">
import { API } from "~/api/api";

const route = useRoute();
const packageUuid = route.params.id.toString();
const { data: packageData } = useQuery(API.getPackage(packageUuid));
const { data: externalIds } = useInfiniteQuery(
    API.getExternalIds({ package: packageUuid }),
);
const artistLink = computed(() =>
    !packageData.value?.artist_id
        ? undefined
        : `/artists/${packageData.value.artist_id}`,
);
const packageReleaseYear = computed(() => packageData?.value?.release_year);
const rating = computed(() => {
    if (!packageData) {
        return undefined;
    }
    if (!externalIds.value) {
        return undefined;
    }
    const firstPage = externalIds.value?.pages.at(0);
    if (firstPage === undefined) {
        return undefined;
    }
    return firstPage.items.find(({ rating }) => rating != null)?.rating ?? null;
});
</script>
<template>
    <ResourcePageHeader
        :poster="packageData?.poster"
        :resource-name="packageData?.name"
    >
        <template #secondary>
            <NuxtLink
                :to="artistLink"
                class="btn btn-ghost btn-sm no-animation"
                :style="{
                    marginLeft: '-0.75rem',
                }"
            >
                <p
                    v-if="packageData"
                    class="prose-lg line-clamp-1 break-all w-full"
                >
                    {{ packageData?.artist_name }}
                </p>
                <div v-else class="skeleton h-6 w-20" />
            </NuxtLink>
        </template>
        <template #ternary>
            <div
                v-if="packageData && rating !== undefined"
                class="flex flex-row"
            >
                <span class="prose-md">
                    {{ packageReleaseYear?.getFullYear() }}</span
                >

                <span
                    v-if="rating !== null && packageReleaseYear"
                    class="prose-md px-2"
                    >{{ "  •  " }}</span
                >
                <Rating v-if="rating !== null" :rating="rating" />
            </div>
            <div v-else class="skeleton h-6 w-20" />
        </template>
    </ResourcePageHeader>
</template>
