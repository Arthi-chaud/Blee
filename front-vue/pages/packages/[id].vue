<script setup lang="ts">
import { API } from "~/api/api";

const route = useRoute();
const { data: packageData } = useQuery(
    API.getPackage(route.params.id.toString()),
);
</script>
<template>
    <ResourcePageHeader
        :poster="packageData?.poster"
        :resource-name="packageData?.name"
    >
        <template #secondary>
            <!--TODO add button-->
            {{ packageData?.artist_name }}
        </template>
        <template #ternary>
            <!--TODO add ratings + release year-->
            {{ packageData?.artist_name }}
        </template>
    </ResourcePageHeader>
</template>
