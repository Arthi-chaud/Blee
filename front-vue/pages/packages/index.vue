<script setup lang="ts">
import { API } from "~/api/api";
import { useQuery } from "~/composables/queries";

const { data, suspense } = useQuery(
    API.getPackages({}, { sortBy: "name", order: "asc" }),
);

onServerPrefetch(async () => {
    await suspense();
});
</script>
<template>
    <div class="poster-grid w-full">
        <div v-for="item in data?.items" :key="item.id">
            <PackageItem :package="item" />
        </div>
    </div>
</template>
