<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getExtras({}, { sort: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll
        v-slot="{ item }"
        :query="query"
        type="thumbnail"
        direction="vertical"
    >
        <ExtraItem :extra="item" />
    </InfiniteScroll>
</template>
