import { API } from "~/api/api";
import { useQuery as useTanQuery } from "@tanstack/vue-query";
import type { Image as ImageModel } from "~/models/domain/image";
import type { Package } from "~/models/domain/package";
import type { Chapter } from "~/models/domain/chapter";
import type { Query } from "~/models/queries";
import type { File } from "~/models/domain/file";

export const useResourceMetadata = (
    resourceType: "movie" | "extra",
    resourceUuid: string,
) => {
    const movieData = useQuery(API.getMovie(resourceUuid), {
        enabled: resourceType == "movie",
    });

    const extraData = useQuery(API.getExtra(resourceUuid), {
        enabled: resourceType == "extra",
    });
    const {
        data: chaptersData,
        hasNextPage: hasNextChapterPage,
        fetchNextPage: fetchNextChapterPage,
    } = useInfiniteQuery(API.getChapters(resourceUuid), {
        enabled: resourceType == "movie",
    });
    const packageQuery = ref<Query<Package>>();
    const thumbnail = ref<ImageModel | null>();
    const fileQuery = ref<Query<File>>();
    const chapters = ref<Chapter[] | undefined>([]);
    watch(
        [movieData.data, extraData.data],
        ([m, e]) => {
            const packageId = (m ?? e)?.package_id;
            const fileId = (m ?? e)?.file_id;
            thumbnail.value = (m ?? e)?.thumbnail;
            if (packageId) {
                packageQuery.value = API.getPackage(packageId);
            }
            if (fileId) {
                fileQuery.value = API.getFile(fileId);
            }
        },
        { immediate: true },
    );

    // Fetch all chapters
    watch(
        hasNextChapterPage,
        (hasNextPage) => {
            if (hasNextPage) {
                fetchNextChapterPage();
            }
        },
        { immediate: true },
    );
    watch(
        chaptersData,
        (c) => {
            chapters.value = c?.pages
                .map(toRaw)
                .flatMap(({ items }) => toRaw(items.map(toRaw)));
            // We use immediate to force running the callback even when data is already loaded
        },
        { immediate: true },
    );
    const packageQueryProps = computed(() => ({
        queryKey: packageQuery.value?.queryKey ?? [],
        queryFn: packageQuery.value?.queryFn,
        enabled: !!packageQuery.value,
    }));
    const fileQueryProps = computed(() => ({
        queryKey: fileQuery.value?.queryKey ?? [],
        queryFn: fileQuery.value?.queryFn,
        enabled: !!fileQuery.value,
    }));
    const { data: packageData } = useTanQuery(packageQueryProps);
    const { data: fileData } = useTanQuery(fileQueryProps);

    return {
        thumbnail: thumbnail,
        chapters: chapters,
        movie: movieData.data,
        extra: extraData.data,
        file: fileData,
        parentPackage: packageData,
    };
};
