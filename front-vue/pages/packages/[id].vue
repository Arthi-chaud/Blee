<script setup lang="ts">
import { API } from "~/api/api";
import ExternalIdList from "~/components/external-id-list.vue";

const route = useRoute();
const packageUuid = route.params.id.toString();
const { data: packageData } = useQuery(API.getPackage(packageUuid));
const { data: moviesData } = useInfiniteQuery(
    API.getMovies(
        { package: packageUuid },
        { sort: "releaseDate", order: "asc" },
    ),
);
const extrasQuery = API.getExtras(
    { package: packageUuid },
    { sort: "name", order: "asc" },
);
const extras = useInfiniteQuery(extrasQuery);
const hasExtras = computed(() =>
    !extras.data.value
        ? true
        : (extras.data.value.pages.at(0)?.items ?? []).length > 0,
);
const { data: externalIds } = useInfiniteQuery(
    API.getExternalIds({ package: packageUuid }),
);
const artistLink = computed(() =>
    !packageData.value?.artist_id
        ? undefined
        : `/artists/${packageData.value.artist_id}`,
);
const movies = computed(() =>
    moviesData.value?.pages.map(({ items }) => items).flat(),
);
const packageReleaseYear = computed(() => packageData?.value?.release_year);
const externalIdList = computed(() =>
    externalIds.value?.pages.at(0)?.items.slice(0, 3),
);
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
const packageDescription = computed(() => {
    const firstPage = externalIds.value?.pages.at(0);

    if (firstPage === undefined) {
        return undefined;
    }
    return firstPage.items.find((e) => e.description)?.description ?? null;
});
</script>
<template>
    <div class="h-full w-full max-w-screen-md">
        <ResourcePageHeader
            :poster="packageData?.poster"
            :resource-name="packageData?.name"
            :brief="packageDescription"
        >
            <template #secondary>
                <NuxtLink
                    :to="artistLink"
                    class="btn btn-ghost btn-sm no-animation md:ml-[-0.75rem]"
                >
                    <p
                        v-if="packageData"
                        class="prose-lg font-normal line-clamp-1 w-full"
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
        <div v-if="movies?.length === 1">
            <NuxtLink
                :to="`/player/movie:${movies.at(0)!.id}`"
                class="btn btn-primary btn-sm btn-block mb-3 no-animation"
            >
                <fa icon="play" />
                Play
            </NuxtLink>
        </div>
        <div v-for="movie in movies" :key="movie.slug">
            <div class="w-full flex justify-between">
                <p class="prose-lg">
                    {{ (movies ?? [])?.length > 1 ? movie.name : "Chapters" }}
                </p>
                <div>
                    <NuxtLink
                        v-if="(movies?.length ?? 0) > 1"
                        :to="`/player/movie:${movie.id}`"
                        class="btn btn-primary btn-xs"
                    >
                        <fa icon="play" />
                        Play
                    </NuxtLink>
                </div>
            </div>
            <InfiniteScroll
                v-slot="{ item }"
                :query="API.getChapters(movie.id)"
                type="thumbnail"
                direction="horizontal"
            >
                <ChapterItem :chapter="item" />
            </InfiniteScroll>
        </div>

        <p v-if="(movies ?? [])?.length >= 1 && hasExtras" class="prose-lg">
            Extras
        </p>
        <InfiniteScroll
            v-slot="{ item }"
            :query="extrasQuery"
            type="thumbnail"
            direction="horizontal"
        >
            <ExtraItem
                :extra="item"
                :format-secondary-title="(e) => formatDuration(e.duration)"
            />
        </InfiniteScroll>
        <ExternalIdList :external-ids="externalIdList" />
    </div>
</template>
