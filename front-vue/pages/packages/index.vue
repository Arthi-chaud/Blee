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
    <div>Package  count: {{ data?.items.length }}</div>
</template>
