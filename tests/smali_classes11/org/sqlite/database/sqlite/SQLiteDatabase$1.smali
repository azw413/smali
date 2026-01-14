.class Lorg/sqlite/database/sqlite/SQLiteDatabase$1;
.super Ljava/lang/ThreadLocal;


# annotations
.annotation system Ldalvik/annotation/Signature;
    value = {
        "Ljava/lang/ThreadLocal<",
        "Lorg/sqlite/database/sqlite/SQLiteSession;",
        ">;"
    }
.end annotation


# instance fields
.field final synthetic QxtthM:Lorg/sqlite/database/sqlite/SQLiteDatabase;


# direct methods
.method constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V
    .locals 0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase$1;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-direct {p0}, Ljava/lang/ThreadLocal;-><init>()V

    return-void
.end method


# virtual methods
.method protected bridge synthetic initialValue()Ljava/lang/Object;
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase$1;->initialValue()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    return-object v0
.end method

.method protected initialValue()Lorg/sqlite/database/sqlite/SQLiteSession;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase$1;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->createSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    return-object v0
.end method
