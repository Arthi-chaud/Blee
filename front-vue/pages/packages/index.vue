<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getPackages({}, { sortBy: "name", order: "asc" });

</script>
<template>
    <InfiniteScroll :query="query" type="poster" v-slot="{ item }">
        <PackageItem :package="item"/>
    </InfiniteScroll>
</template>
