.class final Lorg/greenrobot/eventbus/Subscription;
.super Ljava/lang/Object;
.source "Subscription.java"


# instance fields
.field volatile active:Z

.field final subscriber:Ljava/lang/Object;

.field final subscriberMethod:Lorg/greenrobot/eventbus/SubscriberMethod;


# direct methods
.method constructor <init>(Ljava/lang/Object;Lorg/greenrobot/eventbus/SubscriberMethod;)V
    .locals 1
    .param p1, "subscriber"    # Ljava/lang/Object;
    .param p2, "subscriberMethod"    # Lorg/greenrobot/eventbus/SubscriberMethod;

    .line 27
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 28
    iput-object p1, p0, Lorg/greenrobot/eventbus/Subscription;->subscriber:Ljava/lang/Object;

    .line 29
    iput-object p2, p0, Lorg/greenrobot/eventbus/Subscription;->subscriberMethod:Lorg/greenrobot/eventbus/SubscriberMethod;

    .line 30
    const/4 v0, 0x1

    iput-boolean v0, p0, Lorg/greenrobot/eventbus/Subscription;->active:Z

    .line 31
    return-void
.end method


# virtual methods
.method public equals(Ljava/lang/Object;)Z
    .locals 4
    .param p1, "other"    # Ljava/lang/Object;

    .line 35
    instance-of v0, p1, Lorg/greenrobot/eventbus/Subscription;

    const/4 v1, 0x0

    if-eqz v0, :cond_1

    .line 36
    move-object v0, p1

    check-cast v0, Lorg/greenrobot/eventbus/Subscription;

    .line 37
    .local v0, "otherSubscription":Lorg/greenrobot/eventbus/Subscription;
    iget-object v2, p0, Lorg/greenrobot/eventbus/Subscription;->subscriber:Ljava/lang/Object;

    iget-object v3, v0, Lorg/greenrobot/eventbus/Subscription;->subscriber:Ljava/lang/Object;

    if-ne v2, v3, :cond_0

    iget-object v2, p0, Lorg/greenrobot/eventbus/Subscription;->subscriberMethod:Lorg/greenrobot/eventbus/SubscriberMethod;

    iget-object v3, v0, Lorg/greenrobot/eventbus/Subscription;->subscriberMethod:Lorg/greenrobot/eventbus/SubscriberMethod;

    .line 38
    invoke-virtual {v2, v3}, Lorg/greenrobot/eventbus/SubscriberMethod;->equals(Ljava/lang/Object;)Z

    move-result v2

    if-eqz v2, :cond_0

    const/4 v1, 0x1

    goto :goto_0

    :cond_0
    nop

    .line 37
    :goto_0
    return v1

    .line 40
    .end local v0    # "otherSubscription":Lorg/greenrobot/eventbus/Subscription;
    :cond_1
    return v1
.end method

.method public hashCode()I
    .locals 2

    .line 46
    iget-object v0, p0, Lorg/greenrobot/eventbus/Subscription;->subscriber:Ljava/lang/Object;

    invoke-virtual {v0}, Ljava/lang/Object;->hashCode()I

    move-result v0

    iget-object v1, p0, Lorg/greenrobot/eventbus/Subscription;->subscriberMethod:Lorg/greenrobot/eventbus/SubscriberMethod;

    iget-object v1, v1, Lorg/greenrobot/eventbus/SubscriberMethod;->methodString:Ljava/lang/String;

    invoke-virtual {v1}, Ljava/lang/String;->hashCode()I

    move-result v1

    add-int/2addr v0, v1

    return v0
.end method
