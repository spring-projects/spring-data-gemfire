/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test.support;

import java.util.concurrent.TimeUnit;

/**
 * The ThreadUtils class is a utility class for working with Java Threads.
 *
 * @author John Blum
 * @see java.lang.Thread
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class ThreadUtils {

	public static boolean sleep(final long milliseconds) {
		try {
			Thread.sleep(milliseconds);
			return true;
		}
		catch (InterruptedException ignore) {
			return false;
		}
	}

	public static void timedWait(final long milliseconds) {
		timedWait(milliseconds, milliseconds);
	}

	public static void timedWait(final long milliseconds, final long interval) {
		timedWait(milliseconds, interval, new WaitCondition() {
			@Override public boolean waiting() {
				return true;
			}
		});
	}

	public static void timedWait(final long milliseconds, long interval, final WaitCondition waitCondition) {
		final long timeout = (System.currentTimeMillis() + milliseconds);

		interval = Math.min(interval, milliseconds);

		while (waitCondition.waiting() && (System.currentTimeMillis() < timeout)) {
			try {
				synchronized (waitCondition) {
					TimeUnit.MILLISECONDS.timedWait(waitCondition, interval);
				}
			}
			catch (InterruptedException ignore) {
			}
		}
	}

	public static interface WaitCondition {
		boolean waiting();
	}

}
