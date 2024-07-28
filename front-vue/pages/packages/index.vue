<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getPackages({}, { sort: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll
        v-slot="{ item }"
        :query="query"
        type="poster"
        direction="vertical"
    >
        <PackageItem :package="item" />
    </InfiniteScroll>
</template>
