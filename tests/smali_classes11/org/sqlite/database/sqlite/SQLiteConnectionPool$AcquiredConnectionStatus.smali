.class final enum Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;
.super Ljava/lang/Enum;


# annotations
.annotation system Ldalvik/annotation/EnclosingClass;
    value = Lorg/sqlite/database/sqlite/SQLiteConnectionPool;
.end annotation

.annotation system Ldalvik/annotation/InnerClass;
    accessFlags = 0x4018
    name = "AcquiredConnectionStatus"
.end annotation

.annotation system Ldalvik/annotation/Signature;
    value = {
        "Ljava/lang/Enum<",
        "Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;",
        ">;"
    }
.end annotation


# static fields
.field private static final synthetic CwUiGf:[Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

.field public static final enum DISCARD:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

.field public static final enum NORMAL:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

.field public static final enum RECONFIGURE:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;


# direct methods
.method static constructor <clinit>()V
    .locals 5

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    const-string v1, "UD{dmg"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    const/4 v2, 0x0

    invoke-direct {v0, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;-><init>(Ljava/lang/String;I)V

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->NORMAL:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    new-instance v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    const-string v2, "INjfbmgj}MO"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const/4 v3, 0x1

    invoke-direct {v1, v2, v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;-><init>(Ljava/lang/String;I)V

    sput-object v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->RECONFIGURE:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    new-instance v2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    const-string v3, "_Bzjmyj"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    const/4 v4, 0x2

    invoke-direct {v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;-><init>(Ljava/lang/String;I)V

    sput-object v2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->DISCARD:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    filled-new-array {v0, v1, v2}, [Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    move-result-object v0

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->CwUiGf:[Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    return-void
.end method

.method private constructor <init>(Ljava/lang/String;I)V
    .locals 0
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()V"
        }
    .end annotation

    invoke-direct {p0, p1, p2}, Ljava/lang/Enum;-><init>(Ljava/lang/String;I)V

    return-void
.end method

.method public static valueOf(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;
    .locals 1

    const-class v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    invoke-static {v0, p0}, Ljava/lang/Enum;->valueOf(Ljava/lang/Class;Ljava/lang/String;)Ljava/lang/Enum;

    move-result-object p0

    check-cast p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    return-object p0
.end method

.method public static values()[Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;
    .locals 1

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->CwUiGf:[Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    invoke-virtual {v0}, [Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->clone()Ljava/lang/Object;

    move-result-object v0

    check-cast v0, [Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    return-object v0
.end method
