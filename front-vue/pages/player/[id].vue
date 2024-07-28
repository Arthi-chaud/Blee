<script setup lang="ts">
import { API } from "~/api/api";
import { useQuery as useTanQuery } from "@tanstack/vue-query";
import type { Query } from "~/models/queries";
import type { Package } from "~/models/domain/package";
import type { File } from "~/models/domain/file";
import type { Image as ImageModel } from "~/models/domain/image";
import Image from "~/components/image.vue";

const ParamSeparator = ":";
type ValidParamPrefix = "extra" | "movie";
definePageMeta({
    layout: "empty",
    validate: async (route) => {
        const param = route.params.id;
        if (typeof param !== "string") {
            return false;
        }
        return param.startsWith("extra:") || param.startsWith("movie:");
    },
});
const route = useRoute();
const { resourceType, resourceId, startTimestamp } = (() => {
    const rawParam = route.params.id as string;
    const splitParam = rawParam.split(ParamSeparator, 2);
    const resourceType: ValidParamPrefix =
        splitParam[0] === "extra" ? "extra" : "movie";
    const resourceId = splitParam[1];
    const rawStartTimestamp = route.query.start as string | undefined;
    const startTimestamp = rawStartTimestamp ? parseInt(rawStartTimestamp) : 0;
    return { resourceType, resourceId, startTimestamp };
})();
const movieData = useQuery(API.getMovie(resourceId), {
    enabled: resourceType == "movie",
});
const extraData = useQuery(API.getExtra(resourceId), {
    enabled: resourceType == "extra",
});
const packageQuery = ref<Query<Package>>();
const thumbnail = ref<ImageModel | null>();
const fileQuery = ref<Query<File>>();
watch([movieData.data, extraData.data], ([m, e]) => {
    const packageId = (m ?? e)?.package_id;
    const fileId = (m ?? e)?.file_id;
    thumbnail.value = (m ?? e)?.thumbnail;
    if (packageId) {
        packageQuery.value = API.getPackage(packageId);
    }
    if (fileId) {
        fileQuery.value = API.getFile(fileId);
    }
});
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
const packageData = useTanQuery(packageQueryProps);
const fileData = useTanQuery(fileQueryProps);
</script>
<template>
    <div class="w-full h-full relative">
        <div class="w-full h-full flex justify-center absolute">
            <!--Rework placement-->
            <Image :image="thumbnail" image-type="thumbnail" />
        </div>
        <div
            class="absolute w-full h-full top-0 bottom-0 z-10 bg-black opacity-60"
        ></div>
        <div
            class="absolute w-full h-top flex justify-center top-0 bottom-0 z-10"
        >
            <span
                class="loading loading-spinner loading-lg text-primary"
            ></span>
        </div>
    </div>
</template>
