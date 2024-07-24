<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getExtras({}, { sortBy: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll :query="query" type="thumbnail" v-slot="{ item }">
        <ExtraItem :extra="item" />
    </InfiniteScroll>
</template>
