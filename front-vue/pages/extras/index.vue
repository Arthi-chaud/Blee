<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getExtras({}, { sortBy: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll v-slot="{ item }" :query="query" type="thumbnail">
        <ExtraItem :extra="item" />
    </InfiniteScroll>
</template>
