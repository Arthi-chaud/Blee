<script setup lang="ts">
const ParamSeparator = ":";
type ValidParamPrefix = "extra" | "movie";
definePageMeta({
    validate: async (route) => {
        const param = route.params.id;
        if (typeof param !== "string") {
            return false;
        }
        return param.startsWith("extra:") || param.startsWith("movie:");
    },
});
const route = useRoute();
const rawParam = route.params.id as string;
const splitParam = rawParam.split(ParamSeparator, 2);
const resourceType: ValidParamPrefix =
    splitParam[0] === "extra" ? "extra" : "movie";
const resourceId = splitParam[1];
const rawStartTimestamp = route.query.start as string | undefined;
const startTimestamp = rawStartTimestamp ? parseInt(rawStartTimestamp) : 0;
</script>
<template>
    <div>
        Playing {{ resourceType }} {{ resourceId }} (starting at
        {{ startTimestamp }})
    </div>
</template>
