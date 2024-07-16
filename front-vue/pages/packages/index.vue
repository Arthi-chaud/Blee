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
    <span>Item count: {{ data?.items.length }}</span>
    <div class="p-4">
        <button class="btn">Hello daisyUI</button>
    </div>
</template>
