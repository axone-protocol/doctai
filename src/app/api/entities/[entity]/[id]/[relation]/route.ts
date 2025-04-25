// app/api/relations/[entity]/[id]/[relation]/route.ts
import { getEntityHandler } from "@/lib/backend/BaseEntityServer";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import "@/lib/backend/registerAllEntities";
import { getEntityNameFromRelationProperty, getRelationPropertyName } from "@/lib/relations";
import { NextRequest, NextResponse } from "next/server";

// Helper

export async function GET(
    request: NextRequest,
    ctx: { params: Promise<{ entity: string; id: string; relation: string }> }
) {
    const { entity, id, relation } = await ctx.params;
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const repo = db.getRepository(entity);
        const relationProp = getRelationPropertyName(entity, relation);
        //---------------------------------
        const record = await repo.findOne({
            where: { id },
            relations: [relationProp],
        });
        //---------------------------------
        if (!record || !(relation in record)) {
            return NextResponse.json({ error: "Not found" }, { status: 404 });
        }
        //---------------------------------
        const related = (record as any)[relationProp];
        const relatedEntity = getEntityNameFromRelationProperty(
            entity,
            relation
        );
        if (!relatedEntity) {
            return NextResponse.json(
                { error: `Unknown relation: ${relation}` },
                { status: 400 }
            );
        }
        const relatedHandler = getEntityHandler(relatedEntity);
        //---------------------------------
        const output = Array.isArray(related)
            ? related.map((r: any) =>
                  relatedHandler?.toDTO ? relatedHandler.toDTO(r) : r
              )
            : relatedHandler?.toDTO?.(related) ?? related;
        return NextResponse.json(output);
    } catch (error) {
        console.error(
            `[Relation API Error]: ${entity}/${id}/${relation}`,
            error
        );
        return NextResponse.json(
            {
                error: "Internal Server Error",
                message:
                    error instanceof Error ? error.message : "Unknown error",
            },
            { status: 500 }
        );
    }
}
