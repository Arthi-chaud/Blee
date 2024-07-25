<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getPackages({}, { sortBy: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll v-slot="{ item }" :query="query" type="poster">
        <PackageItem :package="item" />
    </InfiniteScroll>
</template>
