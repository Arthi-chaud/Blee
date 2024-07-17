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
    <div class="w-96 h-96 bg-red-50">
        <PackageItem :package="data?.items.at(0)"/>
    </div>
    <span>Item count: {{ data?.items.length }}</span>
</template>
