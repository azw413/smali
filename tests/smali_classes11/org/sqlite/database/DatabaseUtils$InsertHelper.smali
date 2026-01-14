.class public Lorg/sqlite/database/DatabaseUtils$InsertHelper;
.super Ljava/lang/Object;


# annotations
.annotation system Ldalvik/annotation/EnclosingClass;
    value = Lorg/sqlite/database/DatabaseUtils;
.end annotation

.annotation system Ldalvik/annotation/InnerClass;
    accessFlags = 0x9
    name = "InsertHelper"
.end annotation

.annotation runtime Ljava/lang/Deprecated;
.end annotation


# static fields
.field static Bqmflp:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static CJEjwX:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static CLxLGm:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static Ckjbzi:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static EIHHeQ:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static QWhCcJ:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field public static final TABLE_INFO_PRAGMA_COLUMNNAME_INDEX:I = 0x1

.field public static final TABLE_INFO_PRAGMA_DEFAULT_INDEX:I = 0x4

.field static TYtzsR:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static YgfVBf:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static eEMtni:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static eWvpNd:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static eegOcW:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static emiOjB:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static mfaOnV:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static vxeYBe:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static xQNgcp:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static xYIdhi:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# instance fields
.field private final BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

.field private CmNvsS:Ljava/util/HashMap;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/HashMap<",
            "Ljava/lang/String;",
            "Ljava/lang/Integer;",
            ">;"
        }
    .end annotation
.end field

.field private final YRAhcR:Ljava/lang/String;

.field private YcpZmW:Ljava/lang/String;

.field private eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

.field private ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

.field private mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Zz(Z)V

    return-void
.end method

.method public constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;)V
    .locals 1

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    const/4 v0, 0x0

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    iput-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    iput-object p2, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YRAhcR:Ljava/lang/String;

    return-void
.end method

.method private QZpJah(Z)Lorg/sqlite/database/sqlite/SQLiteStatement;
    .locals 2

    if-eqz p1, :cond_2

    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

    if-nez p1, :cond_1

    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    if-nez p1, :cond_0

    invoke-direct {p0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->vxVDbt()V

    :cond_0
    new-instance p1, Ljava/lang/StringBuilder;

    invoke-direct {p1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eWvpNd:Ljava/lang/String;

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    const/4 v1, 0x6

    invoke-virtual {v0, v1}, Ljava/lang/String;->substring(I)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->compileStatement(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteStatement;

    move-result-object p1

    iput-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

    :cond_1
    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

    return-object p1

    :cond_2
    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

    if-nez p1, :cond_4

    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    if-nez p1, :cond_3

    invoke-direct {p0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->vxVDbt()V

    :cond_3
    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->compileStatement(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteStatement;

    move-result-object p1

    iput-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

    :cond_4
    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

    return-object p1
.end method

.method public static synthetic Zz(Z)V
    .locals 0
    .annotation system Ldalvik/annotation/MethodParameters;
        accessFlags = {
            0x1000,
            0x1000
        }
        names = {
            "#",
            "#"
        }
    .end annotation

    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Lc;",
            "Lu;",
            "Ln;",
            ")",
            "Lt;"
        }
    .end annotation

    if-eqz p0, :cond_0

    const/4 p0, 0x0

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Zz(Z)V

    :cond_0
    const-string p0, "Es|dq`\u0016\u001d"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->QWhCcJ:Ljava/lang/String;

    const-string p0, ":\u0012"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->emiOjB:Ljava/lang/String;

    const-string p0, "V@B^F\u0013_[rsrlPUY\u001d"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->vxeYBe:Ljava/lang/String;

    const-string p0, "P}q}q`up)),8"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CJEjwX:Ljava/lang/String;

    const-string p0, "j]E\u0011YFEA!fr}IZLX\u0018[RXW\u0003OKCB@]IY\u000eOmXGsV7utLZIV}?{e}loea"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Bqmflp:Ljava/lang/String;

    const-string p0, "V@B^F\u0013SMduulPUY\u001dqAITVWn@\\WW[\u000c\\GY`\u001e\\`Q{s5"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->xQNgcp:Ljava/lang/String;

    const-string p0, "3[^E[\u0013BTcze8\u0019"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YgfVBf:Ljava/lang/String;

    const-string p0, "4\u0012YB\u0014ZXC`zi|"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eEMtni:Ljava/lang/String;

    const-string p0, "?\u0012"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->xYIdhi:Ljava/lang/String;

    const-string p0, "Z|ctfg\u0016|OBO8"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->TYtzsR:Ljava/lang/String;

    const-string p0, "Z|ctfg\u0016zS6R]iw\u007f~}"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eWvpNd:Ljava/lang/String;

    const-string p0, ":\t"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eegOcW:Ljava/lang/String;

    const-string p0, "WSDPVREPTbitJ"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Ckjbzi:Ljava/lang/String;

    const-string p0, "C`qvyr\u0016A`tl}fRP[W\u0007"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->EIHHeQ:Ljava/lang/String;

    const-string p0, "3\u001a"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CLxLGm:Ljava/lang/String;

    const-string p0, "p]\\DY]\u0016\u0012"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mfaOnV:Ljava/lang/String;

    return-void
.end method

.method private mBngTb(Landroid/content/ContentValues;Z)J
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->beginTransactionNonExclusive()V

    :try_start_0
    invoke-direct {p0, p2}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->QZpJah(Z)Lorg/sqlite/database/sqlite/SQLiteStatement;

    move-result-object p2

    invoke-virtual {p2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->clearBindings()V

    invoke-virtual {p1}, Landroid/content/ContentValues;->valueSet()Ljava/util/Set;

    move-result-object v0

    invoke-interface {v0}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v0

    :goto_0
    invoke-interface {v0}, Ljava/util/Iterator;->hasNext()Z

    move-result v1

    if-eqz v1, :cond_0

    invoke-interface {v0}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v1

    check-cast v1, Ljava/util/Map$Entry;

    invoke-interface {v1}, Ljava/util/Map$Entry;->getKey()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Ljava/lang/String;

    invoke-virtual {p0, v2}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->getColumnIndex(Ljava/lang/String;)I

    move-result v2

    invoke-interface {v1}, Ljava/util/Map$Entry;->getValue()Ljava/lang/Object;

    move-result-object v1

    invoke-static {p2, v2, v1}, Lorg/sqlite/database/DatabaseUtils;->bindObjectToProgram(Lorg/sqlite/database/sqlite/SQLiteProgram;ILjava/lang/Object;)V

    goto :goto_0

    :cond_0
    invoke-virtual {p2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->executeInsert()J

    move-result-wide v0

    iget-object p2, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {p2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->setTransactionSuccessful()V
    :try_end_0
    .catch Lorg/sqlite/database/SQLException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->endTransaction()V

    return-wide v0

    :catchall_0
    move-exception p1

    goto :goto_1

    :catch_0
    move-exception p2

    :try_start_1
    sget-object v0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Ckjbzi:Ljava/lang/String;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v2, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->vxeYBe:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object p1

    sget-object v1, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YgfVBf:Ljava/lang/String;

    invoke-virtual {p1, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    iget-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YRAhcR:Ljava/lang/String;

    invoke-virtual {p1, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-static {v0, p1, p2}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    iget-object p1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->endTransaction()V

    const-wide/16 p1, -0x1

    return-wide p1

    :goto_1
    iget-object p2, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {p2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->endTransaction()V

    throw p1
.end method

.method private vxVDbt()V
    .locals 11

    const-string v0, "\'"

    const-string v1, ")"

    new-instance v2, Ljava/lang/StringBuilder;

    const/16 v3, 0x80

    invoke-direct {v2, v3}, Ljava/lang/StringBuilder;-><init>(I)V

    sget-object v4, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->TYtzsR:Ljava/lang/String;

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    iget-object v4, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YRAhcR:Ljava/lang/String;

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    sget-object v4, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CLxLGm:Ljava/lang/String;

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    new-instance v4, Ljava/lang/StringBuilder;

    invoke-direct {v4, v3}, Ljava/lang/StringBuilder;-><init>(I)V

    sget-object v3, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->QWhCcJ:Ljava/lang/String;

    invoke-virtual {v4, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const/4 v3, 0x0

    :try_start_0
    iget-object v5, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->BOqnmN:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    new-instance v6, Ljava/lang/StringBuilder;

    invoke-direct {v6}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v7, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->EIHHeQ:Ljava/lang/String;

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    iget-object v7, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YRAhcR:Ljava/lang/String;

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6, v3}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->rawQuery(Ljava/lang/String;[Ljava/lang/String;)Landroid/database/Cursor;

    move-result-object v3

    new-instance v5, Ljava/util/HashMap;

    invoke-interface {v3}, Landroid/database/Cursor;->getCount()I

    move-result v6

    invoke-direct {v5, v6}, Ljava/util/HashMap;-><init>(I)V

    iput-object v5, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CmNvsS:Ljava/util/HashMap;

    const/4 v5, 0x1

    move v6, v5

    :goto_0
    invoke-interface {v3}, Landroid/database/Cursor;->moveToNext()Z

    move-result v7

    if-eqz v7, :cond_3

    invoke-interface {v3, v5}, Landroid/database/Cursor;->getString(I)Ljava/lang/String;

    move-result-object v7

    const/4 v8, 0x4

    invoke-interface {v3, v8}, Landroid/database/Cursor;->getString(I)Ljava/lang/String;

    move-result-object v8

    iget-object v9, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CmNvsS:Ljava/util/HashMap;

    invoke-static {v6}, Ljava/lang/Integer;->valueOf(I)Ljava/lang/Integer;

    move-result-object v10

    invoke-virtual {v9, v7, v10}, Ljava/util/HashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    invoke-virtual {v2, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v2, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v2, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    if-nez v8, :cond_0

    const-string v7, "?"

    invoke-virtual {v4, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    goto :goto_1

    :cond_0
    sget-object v7, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CJEjwX:Ljava/lang/String;

    invoke-virtual {v4, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v4, v8}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v4, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    :goto_1
    invoke-interface {v3}, Landroid/database/Cursor;->getCount()I

    move-result v7

    if-ne v6, v7, :cond_1

    sget-object v7, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->emiOjB:Ljava/lang/String;

    goto :goto_2

    :cond_1
    sget-object v7, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->xYIdhi:Ljava/lang/String;

    :goto_2
    invoke-virtual {v2, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-interface {v3}, Landroid/database/Cursor;->getCount()I

    move-result v7

    if-ne v6, v7, :cond_2

    sget-object v7, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eegOcW:Ljava/lang/String;

    goto :goto_3

    :cond_2
    sget-object v7, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->xYIdhi:Ljava/lang/String;

    :goto_3
    invoke-virtual {v4, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    add-int/lit8 v6, v6, 0x1

    goto :goto_0

    :cond_3
    if-eqz v3, :cond_4

    invoke-interface {v3}, Landroid/database/Cursor;->close()V

    :cond_4
    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    return-void

    :catchall_0
    move-exception v0

    if-eqz v3, :cond_5

    invoke-interface {v3}, Landroid/database/Cursor;->close()V

    :cond_5
    throw v0
.end method


# virtual methods
.method public bind(ID)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindDouble(ID)V

    return-void
.end method

.method public bind(IF)V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    float-to-double v1, p2

    invoke-virtual {v0, p1, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindDouble(ID)V

    return-void
.end method

.method public bind(II)V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    int-to-long v1, p2

    invoke-virtual {v0, p1, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindLong(IJ)V

    return-void
.end method

.method public bind(IJ)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindLong(IJ)V

    return-void
.end method

.method public bind(ILjava/lang/String;)V
    .locals 1

    if-nez p2, :cond_0

    iget-object p2, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {p2, p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindNull(I)V

    goto :goto_0

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindString(ILjava/lang/String;)V

    :goto_0
    return-void
.end method

.method public bind(IZ)V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    if-eqz p2, :cond_0

    const-wide/16 v1, 0x1

    goto :goto_0

    :cond_0
    const-wide/16 v1, 0x0

    :goto_0
    invoke-virtual {v0, p1, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindLong(IJ)V

    return-void
.end method

.method public bind(I[B)V
    .locals 1

    if-nez p2, :cond_0

    iget-object p2, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {p2, p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindNull(I)V

    goto :goto_0

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindBlob(I[B)V

    :goto_0
    return-void
.end method

.method public bindNull(I)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->bindNull(I)V

    return-void
.end method

.method public close()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

    const/4 v1, 0x0

    if-eqz v0, :cond_0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->ewLXDe:Lorg/sqlite/database/sqlite/SQLiteStatement;

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

    if-eqz v0, :cond_1

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eObTc:Lorg/sqlite/database/sqlite/SQLiteStatement;

    :cond_1
    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YcpZmW:Ljava/lang/String;

    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CmNvsS:Ljava/util/HashMap;

    return-void
.end method

.method public execute()J
    .locals 5

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    if-eqz v0, :cond_0

    const/4 v1, 0x0

    :try_start_0
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->executeInsert()J

    move-result-wide v2
    :try_end_0
    .catch Lorg/sqlite/database/SQLException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    return-wide v2

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    sget-object v2, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Ckjbzi:Ljava/lang/String;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v4, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->xQNgcp:Ljava/lang/String;

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    iget-object v4, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->YRAhcR:Ljava/lang/String;

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-static {v2, v3, v0}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    const-wide/16 v0, -0x1

    return-wide v0

    :goto_0
    iput-object v1, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    throw v0

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    sget-object v1, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->Bqmflp:Ljava/lang/String;

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method public getColumnIndex(Ljava/lang/String;)I
    .locals 3

    const/4 v0, 0x0

    invoke-direct {p0, v0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->QZpJah(Z)Lorg/sqlite/database/sqlite/SQLiteStatement;

    iget-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->CmNvsS:Ljava/util/HashMap;

    invoke-virtual {v0, p1}, Ljava/util/HashMap;->get(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Ljava/lang/Integer;

    if-eqz v0, :cond_0

    invoke-virtual {v0}, Ljava/lang/Integer;->intValue()I

    move-result p1

    return p1

    :cond_0
    new-instance v0, Ljava/lang/IllegalArgumentException;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v2, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mfaOnV:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    sget-object v1, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->eEMtni:Ljava/lang/String;

    invoke-virtual {p1, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-direct {v0, p1}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method public insert(Landroid/content/ContentValues;)J
    .locals 2

    const/4 v0, 0x0

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mBngTb(Landroid/content/ContentValues;Z)J

    move-result-wide v0

    return-wide v0
.end method

.method public prepareForInsert()V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, v0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->QZpJah(Z)Lorg/sqlite/database/sqlite/SQLiteStatement;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->clearBindings()V

    return-void
.end method

.method public prepareForReplace()V
    .locals 1

    const/4 v0, 0x1

    invoke-direct {p0, v0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->QZpJah(Z)Lorg/sqlite/database/sqlite/SQLiteStatement;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mTfErC:Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->clearBindings()V

    return-void
.end method

.method public replace(Landroid/content/ContentValues;)J
    .locals 2

    const/4 v0, 0x1

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/DatabaseUtils$InsertHelper;->mBngTb(Landroid/content/ContentValues;Z)J

    move-result-wide v0

    return-wide v0
.end method
